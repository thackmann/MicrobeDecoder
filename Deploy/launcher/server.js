'use strict';

/**
 * @file Launcher service for Microbe Decoder.
 *
 * This service dynamically provisions Docker containers running the
 * Microbe Decoder Shiny app and routes requests via nginx auth_request.
 *
 * Features:
 * - Warm container pool
 * - Per-session container assignment
 * - CPU-based idle cleanup
 * - HTTP readiness probing
 * - HARD CAP on total containers (active + warm + spawning)
 */

// ============================================================================
// Imports
// ============================================================================
const express = require('express');
const Docker = require('dockerode');
const findFreePort = require('find-free-port');

// ============================================================================
// App + Docker client
// ============================================================================
/**
 * Express application instance.
 * @type {import('express').Express}
 */
const app = express();

// Trust nginx reverse-proxy headers (X-Real-IP / X-Forwarded-For) for req.ip
// Only trust loopback since nginx and this launcher run on the same host.
app.set('trust proxy', 'loopback');

/**
 * Docker client (local socket).
 * @type {Docker}
 */
const docker = new Docker({ socketPath: '/var/run/docker.sock' });

// ============================================================================
// Configuration constants
// ============================================================================

/**
 * Launcher network configuration.
 */
const LAUNCHER_HOST = '127.0.0.1';
const LAUNCHER_PORT = 3000;

/**
 * Docker / Shiny configuration.
 */
const SHINY_PORT_IN_CONTAINER = 3838;
const DOCKER_IMAGE = 'tjhackmann/microbedecoder:latest';
const HOST_JOBS_DIR = '/srv/microbedecoder/jobs';
const CONTAINER_JOBS_DIR = '/srv/shiny-server/jobs';

/**
 * Warm pool policy.
 */
const WARM_POOL_SIZE = 1;
const WARM_POOL_MAX = 2;
const WARM_POOL_REFILL_COOLDOWN_MS = 2000;

/**
 * HARD CAP policy.
 * Total containers managed by this launcher at once:
 * active session containers + warm spares + containers currently being spawned.
 *
 * Set with env var MAX_CONTAINERS_TOTAL (e.g. 5). Default: 25
 */
const MAX_CONTAINERS_TOTAL = Number(process.env.MAX_CONTAINERS_TOTAL || 25);

/**
 * Per-container memory limit policy.
 * Hard RAM cap enforced by Docker cgroups.
 *
 * Set with env var CONTAINER_MEM_GB (default: 8).
 */
const CONTAINER_MEM_GB = Number(process.env.CONTAINER_MEM_GB || 8);
const CONTAINER_MEM_BYTES = Math.floor(CONTAINER_MEM_GB * 1024 ** 3);

/**
 *  Idle detection thresholds for CPU
 */
const IDLE_CPU_THRESHOLD_PCT = 5.0;
const SWEEP_INTERVAL_MS = 30 * 1000;
/**
 * Idle detection thresholds for memory
 */
const IDLE_MEMORY_MIN_BYTES = 325 * 1024 * 1024;         // 325 MB - minimum to be "used"
const IDLE_MEMORY_STABILITY_BYTES = 10 * 1024 * 1024;    // 10 MB - stability threshold
const IDLE_MEMORY_READING_COUNT = 5;                     // Track last 5 readings

/**
 * Idle score definitions
 * 0: Active User (Low CPU, high/varying memory)
 * 1: Monitoring (Slightly stable memory or low CPU)
 * 2: Suspicious (Multiple idle markers triggered)
 * 3: Likely Bot/Ghost (Low CPU + Stable Memory + Minimal Data Loaded)
 */
const IDLE_SCORE_LEVELS = {
  ACTIVE: 0,
  SUSPICIOUS: 2,
  GHOST: 3
};

/**
 * Container Cleanup Policy
 * How load and behavior (score) work together to determine session lifespan.
 */
const CLEANUP_POLICY = {
  // Baseline: How long a perfect user (Score 0) can stay idle.
  // We use a range that slides based on server capacity (Load).
  LIFESPAN_BASE_MS: {
	AT_FULL_CAPACITY: 300 * 1000,   // 5m (server is 100% full)
    AT_ZERO_CAPACITY: 3600 * 1000   // 60m  (server is empty)
  },

  // Behavior Multipliers:
  // We multiply the base lifespan by these factors based on the Idle Score.
  // 1.0 = full time allowed; 0.2 = only 20% of time allowed.
  SCORE_MULTIPLIERS: {
    [IDLE_SCORE_LEVELS.ACTIVE]: 1.0,  // Score 0: 100% of time
    1: 0.75,                          // Score 1: 75% of time
    [IDLE_SCORE_LEVELS.SUSPICIOUS]: 0.5, // Score 2: 50% of time
    [IDLE_SCORE_LEVELS.GHOST]: 0.1    // Score 3: 10% of time (Aggressive)
  }
};

/**
 * Startup readiness policy.
 */
const STARTUP_TIMEOUT_MS = 60_000;

/**
 * Cache interval for expensive docker inspect calls.
 */
const ALIVE_CHECK_EVERY_MS = 10_000;

/**
 * Verbose logging toggle (set VERBOSE=1 to enable).
 * @type {boolean}
 */
const VERBOSE = /^(1|true|yes|on)$/i.test(String(process.env.VERBOSE || ''));


// ============================================================================
// In-memory state
// ============================================================================

/**
 * Active sessions mapped to container metadata.
 * @type {Map<string, Object>}
 */
const activeSessions = new Map();

/**
 * Pool of pre-warmed containers.
 * @type {Array<Object>}
 */
const warmPool = [];

/**
 * Track in-flight session container creations.
 * Prevents concurrent requests for the same sessionId
 * from spawning duplicate md-inst-* containers.
 */
const inflightSessionCreates = new Map();

/**
 * Warm pool refill state.
 */
let refillInProgress = false;
let lastRefillKick = 0;

/**
 * Track spawns in progress (prevents race conditions exceeding MAX_CONTAINERS_TOTAL).
 */
let pendingSpawns = 0;

// ============================================================================
// Small utilities
// ============================================================================

/**
 * Log helper that respects VERBOSE.
 *
 * @param {...any} args - Arguments forwarded to console.log.
 * @returns {void}
 */
function vlog(...args) {
  if (VERBOSE) console.log(...args);
}

/**
 * Return milliseconds elapsed since a timestamp.
 *
 * @param {number} t0 - Start time (Date.now()).
 * @returns {number} Milliseconds elapsed.
 */
function msSince(t0) {
  return Date.now() - t0;
}

/**
 * How many containers are currently "managed" by this launcher.
 * (Running + warm spares + in-progress spawns)
 */
function currentManagedCount() {
  return activeSessions.size + warmPool.length + pendingSpawns;
}

/**
 * Whether we are at/over capacity.
 */
function atCapacity() {
  return currentManagedCount() >= MAX_CONTAINERS_TOTAL;
}

/**
 * Calculate required idle ms based on system Load (Server health) 
 * and User Behavior (Idle Score).
 
 * @returns {number}
 */
function getAllowedIdleMs(score) {
  // 1. Calculate Load-Based Baseline (Linear Interpolation)
  const managed = currentManagedCount();
  const cap = Math.max(1, MAX_CONTAINERS_TOTAL);
  const loadFraction = Math.min(1, Math.max(0, managed / cap));

  const min = CLEANUP_POLICY.LIFESPAN_BASE_MS.AT_FULL_CAPACITY;
  const max = CLEANUP_POLICY.LIFESPAN_BASE_MS.AT_ZERO_CAPACITY;
  
  // As load increases (0 -> 1), the baseline moves from 5m down to 30s.
  const loadBaseline = max + (min - max) * loadFraction;

  // 2. Apply Behavior Multiplier
  const behaviorMultiplier = CLEANUP_POLICY.SCORE_MULTIPLIERS[score] || 0.2;

  return Math.round(loadBaseline * behaviorMultiplier);
}

// ============================================================================
// HTTP readiness helpers
// ============================================================================

/**
 * Perform a simple HTTP GET to trigger Shiny initialization.
 *
 * Note: This treats "any HTTP response" as success (not necessarily 200).
 *
 * @param {string} url - URL to request.
 * @param {number} timeoutMs - Request timeout in milliseconds.
 * @returns {Promise<void>}
 */
function httpGet(url, timeoutMs = 4000) {
  return new Promise((resolve, reject) => {
    const http = require('http');

    const req = http.get(url, { timeout: timeoutMs }, (res) => {
      res.resume();
      resolve();
    });

    req.on('timeout', () => {
      req.destroy(new Error('HTTP GET timeout'));
    });

    req.on('error', reject);
  });
}

/**
 * Poll until an HTTP endpoint becomes reachable.
 *
 * @param {string} url - URL to poll.
 * @param {number} timeoutMs - Maximum wait time.
 * @param {number} pollMs - Polling interval.
 * @returns {Promise<void>}
 */
async function waitForHttpOk(url, timeoutMs, pollMs = 250) {
  const start = Date.now();

  while (true) {
    try {
      await httpGet(url, 2000);
      return;
    } catch (e) {
      if (Date.now() - start > timeoutMs) {
        throw new Error(
          `HTTP not ready after ${timeoutMs} ms for ${url} (last error: ${e.message})`
        );
      }
      await new Promise(r => setTimeout(r, pollMs));
    }
  }
}

// ============================================================================
// Docker helpers
// ============================================================================

/**
 * Check whether a Docker container is running.
 *
 * @param {string} containerId - Docker container ID.
 * @returns {Promise<boolean>}
 */
async function containerIsAlive(containerId) {
  try {
    const info = await docker.getContainer(containerId).inspect();
    return !!info?.State?.Running;
  } catch {
    return false;
  }
}

/**
 * List running containers that were previously managed by this launcher.
 *
 * @returns {Promise<Array<Object>>} Array of dockerode container summary objects.
 */
async function listManagedRunningContainers() {
  try {
    return await docker.listContainers({
      all: false, // running only
      filters: {
        label: ['md.managed=1']
      }
    });
  } catch (e) {
    console.error('[RECONCILE] listContainers failed:', e);
    return [];
  }
}

/**
 * Stop any leftover managed containers from a previous launcher instance.
 *
 * Why:
 * - activeSessions + warmPool are in-memory and reset on launcher restart
 * - old containers can keep running ("ghosts") without being tracked
 *
 * Strategy:
 * - stop all running containers labeled md.managed=1
 * - then warm pool will be rebuilt fresh via ensureWarmPool()
 *
 * @returns {Promise<void>}
 */
async function reconcileOnStartup() {
  console.log('[RECONCILE] Checking for leftover managed containers...');

  const leftovers = await listManagedRunningContainers();

  if (leftovers.length === 0) {
    console.log('[RECONCILE] None found.');
    return;
  }

  console.log(`[RECONCILE] Found ${leftovers.length} leftover managed container(s). Stopping...`);

  for (const c of leftovers) {
    const id = c.Id;
    const name = (c.Names && c.Names[0]) ? c.Names[0] : id.slice(0, 12);

    try {
      console.log(`[RECONCILE] Stopping ${name} (${id.slice(0, 12)})...`);
      await docker.getContainer(id).stop({ t: 5 }).catch(() => {});
    } catch (e) {
      console.error(`[RECONCILE] Failed stopping ${name} (${id.slice(0, 12)}):`, e);
    }
  }

  console.log('[RECONCILE] Done.');
}

/**
 * Compute CPU usage percentage from Docker stats output.
 *
 * @param {Object} stats - Docker stats object.
 * @returns {number} CPU usage percentage.
 */
function cpuPercentFromStats(stats) {
  try {
    const cpu = stats.cpu_stats || {};
    const precpu = stats.precpu_stats || {};

    const cpuDelta =
      (cpu.cpu_usage?.total_usage ?? 0) -
      (precpu.cpu_usage?.total_usage ?? 0);

    const systemDelta =
      (cpu.system_cpu_usage ?? 0) -
      (precpu.system_cpu_usage ?? 0);

    const onlineCpus =
      cpu.online_cpus ??
      (cpu.cpu_usage?.percpu_usage
        ? cpu.cpu_usage.percpu_usage.length
        : 1) ??
      1;

    if (systemDelta <= 0 || cpuDelta < 0) return 0;

    return (cpuDelta / systemDelta) * onlineCpus * 100;
  } catch {
    return 0;
  }
}

/**
 * Retrieve CPU usage percentage for a container.
 *
 * @param {string} containerId - Docker container ID.
 * @returns {Promise<number>}
 */
async function getContainerCpuPercent(containerId) {
  const c = docker.getContainer(containerId);
  const stats = await c.stats({ stream: false });
  return cpuPercentFromStats(stats);
}

/**
 * Retrieve memory usage in bytes for a container
 *
 * Function will return values roughly equal to those from `docker stats`
 * Docker CLI roughly reports:
 *   used = memory_stats.usage - memory_stats.stats.total_inactive_file
 * (cache subtraction; field names vary a bit by cgroup version)
 *
 * @param {string} containerId - Docker container ID.
 * @returns {Promise<number>} Memory usage in bytes (docker-stats-like).
 */
async function getContainerMemoryBytes(containerId) {
  try {
    const c = docker.getContainer(containerId);
    const stats = await c.stats({ stream: false });

    const usage = Number(stats?.memory_stats?.usage ?? 0);

    // cgroup v1 (common): total_inactive_file
    const totalInactiveFile = Number(
      stats?.memory_stats?.stats?.total_inactive_file ?? 0
    );

    // cgroup v2 often exposes inactive_file (some daemons also provide total_inactive_file)
    const inactiveFile = Number(
      stats?.memory_stats?.stats?.inactive_file ?? 0
    );

    // Prefer total_inactive_file if present, else inactive_file, else no subtraction
    const cacheToSubtract = totalInactiveFile > 0 ? totalInactiveFile : inactiveFile;

    // Match docker stats behavior: subtract cache-ish component, never below 0
    const used = Math.max(0, usage - cacheToSubtract);

    return used;
  } catch (e) {
    console.error(`Failed to get memory for ${containerId}:`, e.message);
    return 0;
  }
}


// ============================================================================
// Container orchestration
// ============================================================================

/**
 * Spawn a Docker container and wait until it is HTTP-ready.
 *
 * @param {Object} options
 * @param {string} options.namePrefix - Prefix for container name.
 * @returns {Promise<Object>} Container session metadata.
 */
async function spawnContainer({ namePrefix }) {
  if (atCapacity()) throw new Error(`Capacity reached: managed=${currentManagedCount()} cap=${MAX_CONTAINERS_TOTAL}`);

  pendingSpawns += 1;
  if (currentManagedCount() > MAX_CONTAINERS_TOTAL) {
    pendingSpawns = Math.max(0, pendingSpawns - 1);
    throw new Error(`Capacity reached: managed=${currentManagedCount()} cap=${MAX_CONTAINERS_TOTAL}`);
  }
  const tAll = Date.now();

  let container = null;
  let hostPort = null;
  let name = null;

  try {
    const tPort = Date.now();
    [hostPort] = await findFreePort(4000, 5000);
    name = `${namePrefix}${Date.now()}-${Math.floor(Math.random() * 1e6)}`;

    vlog(`[SPAWN] Creating container ${name} on port ${hostPort}... (findFreePort ${msSince(tPort)} ms)`);

    const tCreate = Date.now();
    container = await docker.createContainer({
      Image: DOCKER_IMAGE,
      name,
      ExposedPorts: { [`${SHINY_PORT_IN_CONTAINER}/tcp`]: {} },
      Labels: {
        'md.managed': '1',
        'md.image': DOCKER_IMAGE,
        'md.role': namePrefix.startsWith('md-spare-') ? 'spare' : 'session',
        'md.createdAt': String(Date.now())
      },
      HostConfig: {
        Binds: [`${HOST_JOBS_DIR}:${CONTAINER_JOBS_DIR}`],
        Memory: CONTAINER_MEM_BYTES,
        MemorySwap: CONTAINER_MEM_BYTES,
        PortBindings: {
          [`${SHINY_PORT_IN_CONTAINER}/tcp`]: [{ HostIp: '127.0.0.1', HostPort: String(hostPort) }]
        },
        AutoRemove: true
      }
    });
    vlog(`[SPAWN] createContainer ok in ${msSince(tCreate)} ms`);

    const tStart = Date.now();
    await container.start();
    vlog(`[SPAWN] container.start ok in ${msSince(tStart)} ms (total ${msSince(tAll)} ms)`);

    const url = `http://127.0.0.1:${hostPort}/`;
    await waitForHttpOk(url, STARTUP_TIMEOUT_MS, 250);

    await new Promise(r => setTimeout(r, 200));
    vlog(`[SPAWN] Container ${name} READY on port ${hostPort} (grand total ${msSince(tAll)} ms)`);

    return {
      containerId: container.id,
      hostPort,
      lastActivity: Date.now(),
      lastAliveCheck: 0,
      lastAlive: true,
      idleSince: null,
      lastCpuCheck: 0,
      lastCpuPct: null,
      createdAt: Date.now(),
      name,
	  lastMemoryBytes: null,
	  peakMemoryBytes: 0,
	  memoryReadings: [],
	  lastMemoryCheck: 0
    };
  } catch (e) {
    // Stop “ready-timeout” or other spawn errors from leaving a running container behind
    if (container) {
      console.error(`[SPAWN] Failed; stopping leaked container ${name || container.id} (port=${hostPort || "?"}):`, e.message || e);
      await docker.getContainer(container.id).stop({ t: 5 }).catch(() => {});
      // AutoRemove=true will remove it after stop
    }
    throw e;
  } finally {
    pendingSpawns = Math.max(0, pendingSpawns - 1);
  }
}

/**
 * Ensure the warm pool contains the configured number of containers.
 *
 * @returns {Promise<void>}
 */
async function ensureWarmPool() {
  const now = Date.now();
  if (refillInProgress) return;
  if (now - lastRefillKick < WARM_POOL_REFILL_COOLDOWN_MS) return;

  lastRefillKick = now;
  refillInProgress = true;

  try {
    // Remove dead spares
    for (let i = warmPool.length - 1; i >= 0; i--) {
      const alive = await containerIsAlive(warmPool[i].containerId);
      if (!alive) {
        vlog(`[POOL] Removing dead spare ${warmPool[i].name || warmPool[i].containerId}`);
        warmPool.splice(i, 1);
      }
    }

    // Trim extras beyond target (defensive: prevents accumulating spares across restarts/races)
    if (warmPool.length > WARM_POOL_SIZE) {
      // Decide which to keep: keep the oldest spares (more likely already "warm"),
      // and stop/remove the newest extras.
      warmPool.sort((a, b) => (a.createdAt || 0) - (b.createdAt || 0));

      const extras = warmPool.splice(WARM_POOL_SIZE); // remove extras from the pool array
      for (const ex of extras) {
        console.log(`[POOL] Trimming extra spare ${ex.name || ex.containerId} (target=${WARM_POOL_SIZE})`);
        await docker.getContainer(ex.containerId).stop({ t: 5 }).catch(() => {});
      }
    }

    // Fill to target, bounded by max AND hard cap
    while (
      warmPool.length < WARM_POOL_SIZE &&
      warmPool.length < WARM_POOL_MAX &&
      !atCapacity()
    ) {
      vlog(
        `[POOL] Warming spare... (current=${warmPool.length}, target=${WARM_POOL_SIZE}, max=${WARM_POOL_MAX}, managed=${currentManagedCount()}/${MAX_CONTAINERS_TOTAL})`
      );
      const spare = await spawnContainer({ namePrefix: 'md-spare-' });
      warmPool.push(spare);
      vlog(`[POOL] Spare ready. pool_size=${warmPool.length} (port=${spare.hostPort}, name=${spare.name})`);
    }

    if (atCapacity() && warmPool.length < WARM_POOL_SIZE) {
      vlog(
        `[POOL] Not filling to target because at capacity (managed=${currentManagedCount()}/${MAX_CONTAINERS_TOTAL})`
      );
    }
  } catch (e) {
    console.error('[POOL] ensureWarmPool error:', e);
  } finally {
    refillInProgress = false;
  }
}

/**
 * Assign a container to a session, using a warm spare if available.
 *
 * @param {string} sessionId - Session identifier.
 * @returns {Promise<Object>} Session container metadata.
 */
async function getOrCreateSessionContainer(sessionId) {
  // If someone else is already creating this session, wait for it.
  if (inflightSessionCreates.has(sessionId)) {
    return await inflightSessionCreates.get(sessionId);
  }

  const p = (async () => {
    // Re-check after await points (defensive)
    const existing = activeSessions.get(sessionId);
    if (existing) return existing;

    if (warmPool.length > 0) {
      const spare = warmPool.shift();
      spare.role = 'session';
      activeSessions.set(sessionId, spare);
      ensureWarmPool().catch(() => {});
      return spare;
    }

    if (atCapacity()) {
      throw new Error(`No capacity for new session: managed=${currentManagedCount()} cap=${MAX_CONTAINERS_TOTAL}`);
    }

    vlog(`[${sessionId}] No warm spare available; spawning on demand...`);
    const info = await spawnContainer({ namePrefix: `md-${sessionId}-` });
    info.role = 'session';
    activeSessions.set(sessionId, info);
    ensureWarmPool().catch(() => {});
    return info;
  })();

  inflightSessionCreates.set(sessionId, p);
  try {
    return await p;
  } finally {
    inflightSessionCreates.delete(sessionId);
  }
}

/**
 * Calculate idle score for a container based on multiple metrics.
 * 
 * Score interpretation:
 * - 0-1: Active user
 * - 2: Possibly idle, monitor closely
 * - 3: Likely idle/bot, cleanup candidate
 *
 * @param {Object} info - Session container info object.
 * @param {number} now - Current timestamp.
 * @returns {number} Idle score (0-3).
 */
function calculateIdleScore(info, now) {
  let score = 0;
  
  // Criterion 1: Low CPU
  // If CPU < 5% = +1
  if (info.lastCpuPct !== null && info.lastCpuPct < IDLE_CPU_THRESHOLD_PCT) {
    score += 1;
  }
  
  // Criterion 2: Stable memory (not changing)
  // Calculate standard deviation of recent memory readings
  if (info.memoryReadings && info.memoryReadings.length >= 3) {
    const readings = info.memoryReadings;
    const mean = readings.reduce((a, b) => a + b, 0) / readings.length;
    const variance = readings.reduce((sum, val) => sum + Math.pow(val - mean, 2), 0) / readings.length;
    const stdDev = Math.sqrt(variance);
    
    // If memory is stable (std dev < 10 MB), it's not being actively used
    if (stdDev < IDLE_MEMORY_STABILITY_BYTES) {
      score += 1;
    }
  }
  
  // Criterion 3: Minimum memory threshold (never used)
  // Peak memory never exceeded minimum = bot likely never loaded any data
  if (info.peakMemoryBytes > 0 && info.peakMemoryBytes < IDLE_MEMORY_MIN_BYTES) {
    score += 1;
  }
  
  return score;
}

// ============================================================================
// Routes
// ============================================================================

/**
 * nginx auth_request endpoint.
 */
app.get('/get_target', async (req, res) => {
  vlog(
    `[DEBUG] Received request for Instance ID: ${req.header('X-Instance-Id')}`
  );
  try {
    const sessionId = req.header('X-Instance-Id');
    vlog(`[DEBUG] Received request for Instance ID: ${sessionId}`);

    if (!sessionId) return res.status(400).send('No instance ID provided');

    let sessionInfo = activeSessions.get(sessionId);

    if (sessionInfo) {
      const now = Date.now();
      const lastCheck = sessionInfo.lastAliveCheck || 0;

      if (now - lastCheck > ALIVE_CHECK_EVERY_MS) {
        sessionInfo.lastAliveCheck = now;
        sessionInfo.lastAlive = await containerIsAlive(sessionInfo.containerId);
      }

      if (!sessionInfo.lastAlive) {
        vlog(`[${sessionId}] Existing container not alive -> dropping session`);
        activeSessions.delete(sessionId);
        sessionInfo = null;
      }
    }

    if (!sessionInfo) {
      sessionInfo = await getOrCreateSessionContainer(sessionId);
    }

    sessionInfo.lastActivity = Date.now();

    vlog(`[${sessionId}] target -> 127.0.0.1:${sessionInfo.hostPort}`);
    res.set('X-Forward-Target', `http://127.0.0.1:${sessionInfo.hostPort}`);
    res.status(200).send('OK');

  } catch (err) {
    const msg = String(err?.message || err || '');

    // If we hit capacity, respond with a clear 503 + Retry-After so nginx/browser can retry.
    if (msg.includes('Capacity reached') || msg.includes('No capacity')) {
      res.set('Retry-After', '5');
      return res.status(503).send('Capacity reached, please retry');
    }

    console.error('Launcher error:', err);
    res.status(503).send('Launcher error');
  }
});

/**
 * Health check endpoint.
 */
app.get('/health', (req, res) => res.status(200).send('OK'));

// ============================================================================
// Periodic jobs
// ============================================================================

/**
 * Periodic idle container cleanup loop.
 * 
 */
setInterval(async () => {
  const now = Date.now();

  for (const [sid, info] of activeSessions.entries()) {
    try {
      // 1. GATHER DATA
      const cpuPct = await getContainerCpuPercent(info.containerId);
      const memoryBytes = await getContainerMemoryBytes(info.containerId);
      
      // Update metadata
      info.lastCpuPct = cpuPct;
      info.lastMemoryBytes = memoryBytes;
      if (memoryBytes > info.peakMemoryBytes) info.peakMemoryBytes = memoryBytes;
      
      // Track memory history (Initializes if empty - fixed "patchy" initialization)
      if (!info.memoryReadings) info.memoryReadings = [];
      info.memoryReadings.push(memoryBytes);
      if (info.memoryReadings.length > IDLE_MEMORY_READING_COUNT) info.memoryReadings.shift();

      // 2. EVALUATE STATE
      const idleScore = calculateIdleScore(info, now);
      
      // Any score above ACTIVE starts the "inactive" timer
      if (idleScore > IDLE_SCORE_LEVELS.ACTIVE) {
        if (info.idleSince === null) info.idleSince = now;
      } else {
        info.idleSince = null;
      }

      // 3. DECIDE CLEANUP
      const inactiveDuration = info.idleSince ? (now - info.idleSince) : 0;
      
      // Calculate individual survival time based on BOTH Score and Load
      const allowedMs = getAllowedIdleMs(idleScore);

      const shouldCleanup = info.idleSince !== null && inactiveDuration >= allowedMs;

      // Log scores
      const cpuLow = (info.lastCpuPct !== null && info.lastCpuPct < IDLE_CPU_THRESHOLD_PCT);
      
      let memMean = null;
      let memStdDev = null;
      let memStable = false;
      
      if (info.memoryReadings && info.memoryReadings.length >= 3) {
        const readings = info.memoryReadings;
        memMean = readings.reduce((a, b) => a + b, 0) / readings.length;
        const variance = readings.reduce((sum, val) => sum + Math.pow(val - memMean, 2), 0) / readings.length;
        memStdDev = Math.sqrt(variance);
        memStable = (memStdDev < IDLE_MEMORY_STABILITY_BYTES);
      }
      
      const peakLow = (info.peakMemoryBytes > 0 && info.peakMemoryBytes < IDLE_MEMORY_MIN_BYTES);
      
      // --- allowedMs component logging (load + multiplier) ---
      const managed = currentManagedCount();
      const cap = Math.max(1, MAX_CONTAINERS_TOTAL);
      const loadFraction = Math.min(1, Math.max(0, managed / cap));
      const min = CLEANUP_POLICY.LIFESPAN_BASE_MS.AT_FULL_CAPACITY;
      const max = CLEANUP_POLICY.LIFESPAN_BASE_MS.AT_ZERO_CAPACITY;
      const loadBaseline = max + (min - max) * loadFraction;
      const behaviorMultiplier = CLEANUP_POLICY.SCORE_MULTIPLIERS[idleScore] || 0.2;
      
      vlog(
        `[${sid}] Score=${idleScore}/3 ` +
        `(+cpuLow=${cpuLow ? 1 : 0}, +memStable=${memStable ? 1 : 0}, +peakLow=${peakLow ? 1 : 0}) | ` +
        `Allowed=${Math.round(allowedMs/1000)}s (baseline=${Math.round(loadBaseline/1000)}s * mult=${behaviorMultiplier}, ` +
        `managed=${managed}/${cap}, load=${loadFraction.toFixed(2)}) | ` +
        `Inactive=${Math.round(inactiveDuration/1000)}s | ` +
        `CPU=${cpuPct.toFixed(1)}% (thr=${IDLE_CPU_THRESHOLD_PCT}%) | ` +
        `Mem=${Math.round((memoryBytes || 0)/1024/1024)}MB ` +
        `Peak=${Math.round((info.peakMemoryBytes || 0)/1024/1024)}MB (min=${Math.round(IDLE_MEMORY_MIN_BYTES/1024/1024)}MB) | ` +
        `StdDev=${memStdDev === null ? 'NA' : Math.round(memStdDev/1024/1024) + 'MB'} (thr=${Math.round(IDLE_MEMORY_STABILITY_BYTES/1024/1024)}MB) ` +
        `n=${info.memoryReadings ? info.memoryReadings.length : 0}`
      );

      if (shouldCleanup) {
        console.log(`[${sid}] UNIFIED CLEANUP: Session expired (Allowed ${Math.round(allowedMs/1000)}s at Score ${idleScore})`);
        await docker.getContainer(info.containerId).stop({ t: 5 }).catch(() => {});
        activeSessions.delete(sid);
        ensureWarmPool().catch(() => {});
      }

    } catch (e) {
      console.error(`[${sid}] Monitor failed:`, e.message);
      await docker.getContainer(info.containerId).stop({ t: 5 }).catch(() => {});
      activeSessions.delete(sid);
      ensureWarmPool().catch(() => {});
    }
  }
}, SWEEP_INTERVAL_MS);

// ============================================================================
// Startup
// ============================================================================

/**
 * Start launcher service.
 */
app.listen(LAUNCHER_PORT, LAUNCHER_HOST, () => {
  console.log(`Launcher running on http://${LAUNCHER_HOST}:${LAUNCHER_PORT}`);
  console.log(`VERBOSE=${VERBOSE ? '1' : '0'}`);

  console.log(
    `Cleanup: dynamic idle time baseline ` +
    `[${CLEANUP_POLICY.LIFESPAN_BASE_MS.AT_FULL_CAPACITY / 1000}s .. ` +
    `${CLEANUP_POLICY.LIFESPAN_BASE_MS.AT_ZERO_CAPACITY / 1000}s] ` +
    `scaled by idle score multipliers; CPU idle threshold ${IDLE_CPU_THRESHOLD_PCT}%; ` +
    `sweep every ${SWEEP_INTERVAL_MS / 1000}s`
  );


  console.log(
    `Warm pool: keep ${WARM_POOL_SIZE} spare(s) READY (max ${WARM_POOL_MAX})`
  );

  console.log(
    `Hard cap: MAX_CONTAINERS_TOTAL=${MAX_CONTAINERS_TOTAL} (managed = activeSessions + warmPool + pendingSpawns)`
  );

  reconcileOnStartup()
    .then(() => ensureWarmPool())
    .catch((e) => console.error('[STARTUP] reconcile/warm error:', e));
});
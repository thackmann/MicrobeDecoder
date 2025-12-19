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
 * Set with env var MAX_CONTAINERS_TOTAL (e.g. 5). Default: 15
 */
const MAX_CONTAINERS_TOTAL = Number(process.env.MAX_CONTAINERS_TOTAL || 15);

/**
 * Per-container memory limit policy.
 * Hard RAM cap enforced by Docker cgroups.
 *
 * Set with env var CONTAINER_MEM_GB (default: 6).
 */
const CONTAINER_MEM_GB = Number(process.env.CONTAINER_MEM_GB || 6);
const CONTAINER_MEM_BYTES = Math.floor(CONTAINER_MEM_GB * 1024 ** 3);

/**
 * Idle container cleanup policy (CPU-based).
 */
const IDLE_CPU_THRESHOLD_PCT = 5.0;
const SWEEP_INTERVAL_MS = 30 * 1000;

/**
 * Dynamic idle timeout policy (varies by load).
 * - When lightly loaded, allow longer idle time.
 * - When near capacity, stop idles faster.
 */
const IDLE_REQUIRED_MS_MIN = 30 * 1000;   // 0.5 min (near capacity)
const IDLE_REQUIRED_MS_MAX = 300 * 1000;  // 5 min (light load)

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
 * Compute required idle ms based on current load.
 *
 * @returns {number}
 */
function idleRequiredMsForLoad() {
  // "managed" includes running sessions + warm spares + pending spawns
  const managed = currentManagedCount();

  // Convert managed count into a 0..1 load fraction vs hard cap
  const cap = Math.max(1, MAX_CONTAINERS_TOTAL);
  const load = Math.min(1, Math.max(0, managed / cap));

  // Linearly interpolate: load=0 => MAX, load=1 => MIN
  const ms = IDLE_REQUIRED_MS_MAX + (IDLE_REQUIRED_MS_MIN - IDLE_REQUIRED_MS_MAX) * load;

  return Math.round(ms);
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
      lowCpuSince: null,
      lastCpuCheck: 0,
      lastCpuPct: null,
      createdAt: Date.now(),
      name
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
 */
setInterval(async () => {
  const now = Date.now();

  for (const [sid, info] of activeSessions.entries()) {
    try {
      const cpuPct = await getContainerCpuPercent(info.containerId);

      info.lastCpuCheck = now;
      info.lastCpuPct = cpuPct;

      if (cpuPct < IDLE_CPU_THRESHOLD_PCT) {
        if (info.lowCpuSince === null) info.lowCpuSince = now;

        const lowForMs = now - info.lowCpuSince;
        const idleRequiredMs = idleRequiredMsForLoad();

        vlog(
          `[${sid}] CPU ${cpuPct.toFixed(2)}% (<${IDLE_CPU_THRESHOLD_PCT}%), ` +
          `lowFor=${Math.round(lowForMs / 1000)}s, ` +
          `required=${Math.round(idleRequiredMs / 1000)}s, ` +
          `managed=${currentManagedCount()}/${MAX_CONTAINERS_TOTAL}`
        );

        if (lowForMs >= idleRequiredMs) {
          console.log(
            `[${sid}] CPU ${cpuPct.toFixed(2)}% < ${IDLE_CPU_THRESHOLD_PCT}% for ` +
            `${Math.round(lowForMs / 1000)}s ` +
            `(required=${Math.round(idleRequiredMs / 1000)}s, managed=${currentManagedCount()}/${MAX_CONTAINERS_TOTAL}) -> stopping`
          );

          await docker.getContainer(info.containerId).stop({ t: 5 }).catch(() => {});
          activeSessions.delete(sid);
          ensureWarmPool().catch(() => {});
        }
      } else {
        info.lowCpuSince = null;
        vlog(`[${sid}] CPU ${cpuPct.toFixed(2)}% (active)`);
      }
    } catch (e) {
      console.log(
        `[${sid}] stats failed -> stopping + removing session (${e?.message || e})`
      );
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
    `Cleanup: stop if CPU < ${IDLE_CPU_THRESHOLD_PCT}% for dynamic idle time in ` +
    `[${IDLE_REQUIRED_MS_MIN / 1000}s .. ${IDLE_REQUIRED_MS_MAX / 1000}s] ` +
    `(sweep every ${SWEEP_INTERVAL_MS / 1000}s)`
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

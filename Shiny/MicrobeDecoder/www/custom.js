// Custom JavaScript for App
// This script defines client-side behavior used across the app, including
// tab navigation, browser history handling, the loading screen, and small
// UI helpers attached to specific modules.
// Author: Timothy Hackmann
// Date: 26 May 2026

// ============================================================================
// Tab navigation and loading screen
// ============================================================================

// Jump to tab selected with buttons on home page
shinyjs.goToTab = function(tabName) {
  $('a[data-value="' + tabName + '"]').click();
  // Close the dropdown synchronously so it never paints open for a frame,
  // with a next-tick repeat as a safety net.
  var closeMenu = function() {
    $('a[data-value="' + tabName + '"]').closest('.dropdown-menu').removeClass('show');
  };
  closeMenu();
  setTimeout(closeMenu, 0);
};

// Handle the browser back and forward buttons
// When the user presses back or forward, swap to the requested tab on the
// client side rather than asking the server, so the new tab appears
// without a websocket round trip.  A flag is set first so the server
// does not push a duplicate URL in response to the resulting tab change.
// If a modal is open when the navigation happens, it is dismissed and
// any module listening for that event is notified.
(function() {
  function getTabFromQuery() {
    var match = window.location.search.match(/[?&]tab=([^&]*)/);
    return match ? decodeURIComponent(match[1]) : 'home';
  }

  function removeBlockingModalAfterHistoryNavigation(tab) {
    var hasModal = document.querySelector('.modal.show, .modal') !== null;
    var hasBackdrop = document.querySelector('.modal-backdrop') !== null;
    if (!hasModal && !hasBackdrop) return false;

    console.warn('[nav-debug] Browser history navigation occurred while a modal was mounted. Removing modal immediately.', {
      tab: tab, href: window.location.href, hasModal: hasModal, hasBackdrop: hasBackdrop
    });
    document.querySelectorAll('.modal').forEach(function(el) { el.remove(); });
    document.querySelectorAll('.modal-backdrop').forEach(function(el) { el.remove(); });
    document.body.classList.remove('modal-open');
    document.body.style.removeProperty('padding-right');
    document.body.style.removeProperty('overflow');
    return true;
  }

  function showTabViaBootstrap(tab) {
    // Find the nav link for the requested tab.
    var navLink = document.querySelector('a[data-value="' + tab + '"]')
               || document.querySelector('[data-bs-toggle="tab"][data-value="' + tab + '"]');
    if (!navLink) {
      console.warn('[nav-debug] popstate: no nav link found for tab', tab);
      return false;
    }
    // Prefer the Bootstrap tab API, falling back to jQuery, then a click.
    if (window.bootstrap && bootstrap.Tab) {
      var inst = bootstrap.Tab.getInstance(navLink) || new bootstrap.Tab(navLink);
      inst.show();
    } else if (window.jQuery) {
      $(navLink).tab('show');
    } else {
      navLink.click();
    }
    // Close the parent dropdown menu if the tab lives in a nav_menu().
    // Run synchronously so the menu never paints open for a frame, then once
    // more on the next tick as a safety net in case Bootstrap re-opens it.
    var closeDropdown = function() {
      var dropdownMenu = navLink.closest && navLink.closest('.dropdown-menu');
      if (dropdownMenu) dropdownMenu.classList.remove('show');
      // Also reset the dropdown toggle so its arrow returns to closed.
      var dropdownToggle = dropdownMenu && dropdownMenu.previousElementSibling;
      if (dropdownToggle && dropdownToggle.classList.contains('dropdown-toggle')) {
        dropdownToggle.setAttribute('aria-expanded', 'false');
      }
    };
    closeDropdown();
    setTimeout(closeDropdown, 0);
    return true;
  }

  function onPopstate() {
    var tab = getTabFromQuery();

    if (!window.Shiny || !Shiny.setInputValue) {
      // Shiny is not ready yet; try again shortly.
      setTimeout(onPopstate, 50);
      return;
    }

    var removedModal = removeBlockingModalAfterHistoryNavigation(tab);

    // Signal the server to skip the URL push it would otherwise make in
    // response to the tab change.
    Shiny.setInputValue('nav_from_history_arm', {
      tab: tab, time: Date.now()
    }, {priority: 'event'});

    // Notify any module that was showing a modal so it can clean up.
    if (removedModal) {
      Shiny.setInputValue('nav_modal_force_closed', {
        tab: tab, href: window.location.href, time: Date.now()
      }, {priority: 'event'});
    }

    console.log('[nav-debug] popstate ->', tab, {removedModal: removedModal});
    showTabViaBootstrap(tab);
  }

  window.addEventListener('popstate', onPopstate);
})();

// Push the URL and show the loading screen on every tab change
// The URL is pushed from the browser as soon as a tab change starts, so
// the back button always finds a history entry for the new tab. The app
// loading screen remains visible until the requested module signals that
// it is ready.
(function() {
  var APP_TABS = new Set([
    'home',
    'predictionsTaxonomy',
    'predictionsNetwork',
    'predictionsMachineLearning',
    'history',
    'databaseSearch',
    'databaseDownload',
    'help',
    'about'
  ]);

  // Give each navbar link a meaningful URL.
  // The app normally stores the internal Bootstrap panel target in href, such as
  // "#tab-8864-1". That works for a normal click, but it gives the wrong
  // address when the user opens a link in a new tab or window.
  // Preserve the internal panel target in data-bs-target, then use href for
  // the shareable ?tab= URL.
  function makeNavbarLinksOpenable() {
    document.querySelectorAll('#tabs a[data-value]').forEach(function(link) {
      var tab = link.getAttribute('data-value');

      // Ignore nested module tabs and any other non-app links.
      if (!APP_TABS.has(tab)) return;

      // Preserve the internal Bootstrap panel target before replacing href.
      var panelTarget =
        link.getAttribute('data-bs-target') ||
        link.getAttribute('href');

      if (panelTarget && panelTarget.charAt(0) === '#') {
        link.setAttribute('data-bs-target', panelTarget);
      }

      // Use the current app path so this also works when the app is deployed
      // below a subdirectory rather than at the site root.
      var url = new URL(window.location.href);
      url.search = '?tab=' + encodeURIComponent(tab);
      url.hash = '';

      link.setAttribute('href', url.pathname + url.search);
    });
  }

  // custom.js is loaded in the document head, before the navbar exists.
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', makeNavbarLinksOpenable);
  } else {
    makeNavbarLinksOpenable();
  }

  // Identify the tab requested by the URL when the page first loads.
  // The app initially opens Home as a placeholder, but a saved-job URL may
  // request another tab. Keep the loading screen visible until that requested
  // tab is ready.
  var initialParams = new URLSearchParams(window.location.search);
  var initialTargetTab = initialParams.get('tab') || 'home';

  if (!APP_TABS.has(initialTargetTab)) {
    initialTargetTab = 'home';
  }

  var waitingForInitialTarget = initialTargetTab !== 'home';

  // Push the URL for a top-level app tab change.
  function pushUrlForTab(tab) {
    var currentParams = new URLSearchParams(window.location.search);

    // Keep the current URL when it already points to the requested tab.
    // This preserves the user and job parameters when the app opens a saved
    // result from the History tab or from a shared link.
    if (currentParams.get('tab') === tab) return;

    // When the user manually moves to a different tab, create a clean URL.
    // This removes parameters associated with the previous tab.
    var desired = '?tab=' + encodeURIComponent(tab);

    try {
      history.pushState({}, '', desired);
      console.log('[nav-debug] url pushed (client):', tab);
    } catch (err) {
      console.warn('[nav-debug] url push failed:', err);
    }
  }

  // Show and hide the app loading screen.
  // The loading screen and the wrapper containing the app content are both
  // created at startup. Showing one and hiding the other prevents partially
  // loaded modules from appearing briefly during navigation.
  var LOADING_MAX_DURATION_MS = 15000;
  var loadingAutoHideTimer = null;

  function showLoadingScreen(reason) {
    var loading = document.getElementById('app-loading-screen');
    var wrapper = document.getElementById('app-wrapper');
    if (!loading || !wrapper) return;

    loading.classList.remove('shinyjs-hide');
    wrapper.classList.add('shinyjs-hide');
    console.log('[nav-debug] loading screen shown:', reason);

    // Arm the auto-hide so the user is not locked out if the module never
    // signals that it is ready.
    if (loadingAutoHideTimer != null) clearTimeout(loadingAutoHideTimer);

    loadingAutoHideTimer = setTimeout(function() {
      console.warn(
        '[nav-debug] loading screen auto-hide timeout (' +
        LOADING_MAX_DURATION_MS +
        'ms)'
      );

      hideLoadingScreen('auto_hide_timeout');
    }, LOADING_MAX_DURATION_MS);
  }

  function hideLoadingScreen(reason) {
    var loading = document.getElementById('app-loading-screen');
    var wrapper = document.getElementById('app-wrapper');
    if (!loading || !wrapper) return;

    loading.classList.add('shinyjs-hide');
    wrapper.classList.remove('shinyjs-hide');
    console.log('[nav-debug] loading screen hidden:', reason);

    if (loadingAutoHideTimer != null) {
      clearTimeout(loadingAutoHideTimer);
      loadingAutoHideTimer = null;
    }
  }

  // Called by R once a module has finished loading.
  window.notifyModuleReady = function(tabName) {
    // During startup from a saved-job URL, Home loads first only because it is
    // the placeholder tab. Do not reveal it while waiting for the tab requested
    // by the URL.
    if (waitingForInitialTarget && tabName !== initialTargetTab) {
      console.log(
        '[nav-debug] ignoring placeholder module_ready:' +
        tabName +
        '; waiting for initial tab:' +
        initialTargetTab
      );

      return;
    }

    waitingForInitialTarget = false;
    hideLoadingScreen('module_ready:' + tabName);
  };

  // Only top-level app tabs should affect the browser URL. Nested tabs inside
  // modules, such as Heatmap, Treemap, and Database matches, are ignored here.
  document.addEventListener('hide.bs.tab', function(e) {
    var destTab =
      e.relatedTarget &&
      e.relatedTarget.getAttribute('data-value');

    if (!destTab || !APP_TABS.has(destTab)) return;

    pushUrlForTab(destTab);
    // Navigation now relies on each module's own loading screen, which sits
    // below the navbar, so the navbar stays visible during a tab change.
    // The full-screen overlay below is left for startup only.
    // showLoadingScreen('hide.bs.tab:' + destTab);
  }, true);
})();

// Jump to a panel within the Help tab
// Switches to the Help tab, then selects the requested subtab pill once the
// Help module has loaded.  Polling is needed because the Help module is
// transient and its pills do not exist until after the tab opens.
shinyjs.goToHelpPanel = function(panel) {
  // Close any open modal (the link lives inside one).
  document.querySelectorAll('.modal, .modal-backdrop').forEach(function(el) { el.remove(); });
  document.body.classList.remove('modal-open');
  document.body.style.removeProperty('padding-right');
  document.body.style.removeProperty('overflow');

  //Navigate to specified panel
  $('a[data-value="help"]').click();
  var tries = 0;
  (function selectPanel() {
    var pill = $('#help-subtabs a[data-value="' + panel + '"]');
    if (pill.length) {
      pill.click();
    } else if (tries++ < 40) {
      setTimeout(selectPanel, 50);
    }
  })();
};

// ============================================================================
// Animations
// ============================================================================

//Animation for loading screen
document.addEventListener("DOMContentLoaded", function () {
  const fadeSvgElements = document.querySelectorAll(".fade-svg");

  fadeSvgElements.forEach((el) => {
    el.style.opacity = 0;
    el.style.animationName = "fadeInOut";
    el.style.animationDuration = "2s";
    el.style.animationDelay = "0.2s";
    el.style.animationIterationCount = "infinite";
    el.style.animationTimingFunction = "ease-in-out";
    el.style.animationFillMode = "forwards";
  });
});

//Animation for logo subtitle
window.hasRunTypingEffect = false;

Shiny.addCustomMessageHandler('triggerTypingEffect', function(tab) {
  if (tab !== 'home' || window.hasRunTypingEffect) return;
  window.hasRunTypingEffect = true;

  const target = document.querySelector('[data-text]');
  if (!target) return;

  const text = target.getAttribute('data-text');
  target.innerHTML = '';

  // Create span for typed text with cursor
  const textSpan = document.createElement('span');
  Object.assign(textSpan.style, {
    display: 'inline-block',
    minWidth: '39ch', // gives text space to be typed out
    textAlign: 'left',
    verticalAlign: 'baseline',
  });

  // Create blinking cursor
  const cursor = document.createElement('span');
  cursor.textContent = '|';
  Object.assign(cursor.style, {
    display: 'inline-block',
    animation: 'blink 1s step-start infinite',
    marginLeft: '1px'
  });

  // Add cursor inside textSpan
  textSpan.appendChild(cursor);
  target.appendChild(textSpan);

 // Typing logic
  let i = 0;
  function typeChar() {
    if (i < text.length) {
      const charNode = document.createTextNode(text.charAt(i));
      textSpan.insertBefore(charNode, cursor);
      i++;
      setTimeout(typeChar, 30);
    } else {
      // After typing finishes, wait 0.5 seconds then hide the cursor
      setTimeout(() => {
        cursor.style.display = 'none';
      }, 500);
    }
  }

  typeChar();
});

// ============================================================================
// Session lifecycle
// ============================================================================

// Redirect to a static page if the Shiny session disconnects
$(document).on('shiny:disconnected', function () {
  window.location.replace('/disconnected.html');
});

// Retrieve user's public IP address on session init and expose as input$user_ip
$(document).on("shiny:sessioninitialized", function () {
  $.get("https://api.ipify.org", function (response) {
    Shiny.setInputValue("user_ip", response);
  });
});

// ============================================================================
// Resize plots
// ============================================================================

// Dynamically resize width based on height
shinyjs.resizeWidthFromHeight = function(containerId, heightToWidthRatio) {
  function resizeContainer() {
    var height = $('#' + containerId).height();
    var newWidth = height * heightToWidthRatio;
    $('#' + containerId).css('width', newWidth + 'px');
  }

  resizeContainer();  // Initial
  $(window).resize(resizeContainer);  // On window resize
};

// Re-measure a slider that was built inside a hidden panel.
// ion.rangeSlider reads its container width when it is created, so a slider that
// starts out inside a hidden conditionalPanel comes out zero-width and the handle
// sits pinned to the left.  Calling update() once the panel is on screen makes the
// slider measure itself again.
shinyjs.refreshSlider = function(inputId) {
  setTimeout(function() {
    var slider = $('#' + inputId).data('ionRangeSlider');
    if (slider) slider.update();
  }, 0);
};

// Truncate native plotly tick labels (x and/or y) to the live pixel budget,
// re-running on resize so widening the plot restores characters instead of
// leaving the ellipsis stranded. The full labels are supplied by R (or read
// from the layout) and stashed on the element, so each pass truncates from the
// original rather than from an already-shortened label. Hover is unaffected:
// only the displayed ticktext changes, not the trace data.
//
//   opts.axes : axes to handle, e.g. ['x'] or ['x','y']        (default ['x'])
//   opts.full : { x: [...], y: [...] } full labels from R      (optional; falls
//               back to whatever ticktext is already in the layout)
//   opts.pad  : pixels of breathing room inside each budget    (default 10)
//
// x labels each own one category column (budget = column width); y labels share
// the left margin (budget = the gutter between the paper edge and the plot area).
function truncateAxisTickLabels(el, opts) {
  opts = opts || {};
  var axes = opts.axes || ['x'];
  var full = opts.full || {};
  var PAD = opts.pad != null ? opts.pad : 10;
  var ELL = '\u2026';

  // Refresh the stash per axis on every render. Shiny reuses the same DOM node
  // for an output across re-renders, so `el` (and anything stashed on it)
  // survives. When R supplies authoritative full labels we MUST overwrite, or a
  // new plot rendered into the same element keeps replaying the previous plot's
  // labels. Only the layout-read fallback is stashed once, before our own
  // truncation has rewritten ticktext to the shortened form.
  axes.forEach(function (ax) {
    var key = '_full_' + ax;
    if (full[ax]) {
      el[key] = full[ax].slice();
    } else if (!el[key]) {
      var la = (el.layout && el.layout[ax + 'axis']) || {};
      el[key] = (la.ticktext || []).slice();
    }
  });

  // Measure against the font actually rendered for this axis' ticks.
  function measurer(ax) {
    var t = el.querySelector('.' + ax + 'tick text');
    var font = t ? getComputedStyle(t).font : '12px sans-serif';
    var ctx = (el._measureCanvas = el._measureCanvas || document.createElement('canvas')).getContext('2d');
    ctx.font = font;
    return function (s) { return ctx.measureText(s).width; };
  }

  function shorten(text, budget, measure) {
    if (measure(text) <= budget) return text;
    var b = budget - measure(ELL);
    if (b <= 0) return ELL;
    var lo = 0, hi = text.length, best = 0;
    while (lo <= hi) {
      var mid = (lo + hi) >> 1;
      if (measure(text.slice(0, mid)) <= b) { best = mid; lo = mid + 1; }
      else { hi = mid - 1; }
    }
    return best ? text.slice(0, best) + ELL : ELL;
  }

  function budgetFor(fl, ax) {
    if (ax === 'x') {
      var xa = fl.xaxis;
      return Math.abs(xa.d2p(1) - xa.d2p(0));   // one category column, in px
    }
    return fl.xaxis._offset;                     // px from paper edge to plot area
  }

  function retrunc() {
    var fl = el._fullLayout;
    if (!fl) return;
    var patch = {}, changed = false;
    axes.forEach(function (ax) {
      var fullLabels = el['_full_' + ax];
      if (!fullLabels || !fullLabels.length) return;
      var span = budgetFor(fl, ax);
      if (!span) return;
      var measure = measurer(ax);
      var budget = span - PAD;
      var next = fullLabels.map(function (s) { return shorten(s, budget, measure); });
      var cur = (fl[ax + 'axis'].ticktext) || [];
      if (next.length === cur.length && next.every(function (v, i) { return v === cur[i]; })) return;
      patch[ax + 'axis.ticktext'] = next;
      changed = true;
    });
    if (!changed) return;
    el._truncBusy = true;
    Plotly.relayout(el, patch).then(function () { el._truncBusy = false; });
  }

  // One debounced observer per element; replace any prior one if reused.
  if (el._truncRO) el._truncRO.disconnect();
  var timer;
  el._truncRO = new ResizeObserver(function () {
    if (el._truncBusy) return;          // ignore the resize our own relayout triggers
    clearTimeout(timer);
    timer = setTimeout(retrunc, 120);
  });
  el._truncRO.observe(el);

  retrunc();   // initial pass
}


// ============================================================================
// Module-specific helpers
// ============================================================================

// Define handlers for checkboxes in History module
function registerJobTableHandlers(jobTableId) {
  // Remember which jobs are checked so a selection survives the live table
  // redraws used for progress updates. Without this, every redraw would clear
  // the user's checkboxes.
  var selectedJobs = {};

  // Record the check state as the user toggles individual boxes. The handler is
  // delegated from the document so it keeps working after the table redraws.
  $(document)
    .off('change.jobTable')
    .on('change.jobTable', '#' + jobTableId + ' input.row_checkbox', function () {
      if (this.checked) {
        selectedJobs[this.value] = true;
      } else {
        delete selectedJobs[this.value];
      }
    });

  // Restore the check state after every redraw, including live progress updates.
  $('#' + jobTableId)
    .off('draw.dt.jobTable')
    .on('draw.dt.jobTable', function () {
      document.querySelectorAll('#' + jobTableId + ' input.row_checkbox').forEach(function (el) {
        el.checked = !!selectedJobs[el.value];
      });
    });

  Shiny.addCustomMessageHandler(jobTableId + '_get_selected', function(message) {
    var selected = [];
    document.querySelectorAll('input[name="row_selected"]:checked').forEach(function(el) {
      selected.push(el.value);
    });
    Shiny.setInputValue(jobTableId + '_selected_jobs', selected, {priority: 'event'});
  });

  Shiny.addCustomMessageHandler(jobTableId + '_toggle_checkboxes', function(checked) {
    $('input.row_checkbox').prop('checked', checked);
    // Keep the remembered selection in sync with the toggle.
    document.querySelectorAll('input.row_checkbox').forEach(function (el) {
      if (checked) {
        selectedJobs[el.value] = true;
      } else {
        delete selectedJobs[el.value];
      }
    });
  });
}

// Handle clicks for fileInput_link object
// Any element with class .file-input-link-trigger will
// forward its click to the hidden file input whose id is given in the data-target attribute.
$(document).on('click', '.file-input-link-trigger', function() {
  var targetId = $(this).data('target');
  if (targetId) {
    $('#' + targetId).click();
  }
});

// Handle clicks for the clear link of fileInput_modal
// Any element with class .file-input-clear-trigger will clear the file input
// whose id is given in the data-target attribute.  Shiny has no server-side
// way to reset a file input, so the file element, the file name box, and the
// progress bar are all cleared here, and the input value is set to null so
// the server sees no file.
$(document).on('click', '.file-input-clear-trigger', function(e) {
  e.preventDefault();
  var targetId = $(this).data('target');
  if (targetId) {
    var $container = $('#' + targetId).closest('.shiny-input-container');
    // Clearing the value matters even though the box looks empty.  Without it,
    // choosing the same file again fires no change event and nothing uploads.
    $('#' + targetId).val('');
    $container.find('input[type="text"]').val('');
    $container.find('.progress-bar').css('width', '0%').text('');
    Shiny.setInputValue(targetId, null, {priority: 'event'});
    $(this).hide();
  }
});

// Show the clear link of fileInput_modal once a file is chosen
// The link is rendered hidden, so it is revealed when the file input named in
// its data-target attribute reports a selected file.
$(document).on('change', '.shiny-input-container input[type="file"]', function() {
  var hasFile = this.files && this.files.length > 0;
  $('.file-input-clear-trigger[data-target="' + this.id + '"]').toggle(hasFile);
});

// Style queryBuilder rules that use the selectize plugin
// When a queryBuilder rule is added, if its filter uses the selectize plugin,
// widen the value container and strip Bootstrap's form-select class from the
// selectize wrapper/dropdown so the selectize widget renders correctly.
function registerQueryBuilderSelectizeStyling(queryBuilderId) {
  $('#' + queryBuilderId).on('afterCreateRuleInput.queryBuilder', function(e, rule) {
    if (rule.filter.plugin == 'selectize') {
      var $valueContainer = rule.$el.find('.rule-value-container');
      $valueContainer.css('min-width', '10vw');
      $valueContainer.find('.selectize-control').removeClass('form-select');
      $valueContainer.find('.selectize-dropdown').removeClass('form-select');
    }
  });
}

// Signal when queryBuilder is ready
function registerQueryBuilderFiltersSetSignal(queryBuilderId) {
  $('#' + queryBuilderId).on('afterSetFilters.queryBuilder', function() {
    Shiny.setInputValue(queryBuilderId + '_ready', Math.random());
  });
}

// ============================================================================
// Plot options accordion
// ============================================================================

// Close the plot options accordion when the tab changes
// The options belong to the tab that is open, so an accordion left open on one
// tab shows a mostly empty body on the next. Closing it through bslib would
// animate the close and need a server round trip, so the classes are edited
// directly instead. Bootstrap reads the open state off the show class, so
// dropping that class leaves the accordion in a state the next click handles
// correctly. The collapsing class and the inline height are cleared too, in
// case the tab changes mid-animation.
function closeOptionsAccordions() {
  document.querySelectorAll('.options-accordion .accordion-collapse').forEach(function(panel) {
    if (!panel.classList.contains('show') && !panel.classList.contains('collapsing')) {
      return;
    }

    panel.classList.remove('show', 'collapsing');
    panel.classList.add('collapse');
    panel.style.height = '';

    var item = panel.parentElement;
    var button = item ? item.querySelector('.accordion-button') : null;

    if (button) {
      button.classList.add('collapsed');
      button.setAttribute('aria-expanded', 'false');
    }
  });
}

document.addEventListener('show.bs.tab', closeOptionsAccordions);

// ============================================================================
// Spinners
// ============================================================================

// Mark an output once it has rendered, which keeps its spinner hidden from
// then on.  Only outputs wrapped with fires_once carry the data attribute.
$(document).on('shiny:value', function(event) {
  $(event.target)
    .closest('.shiny-spinner-output-container[data-spin-once]')
    .addClass('spinner-rendered');
});

// Clear the marks for one module so its spinners fire again for a new job
Shiny.addCustomMessageHandler('resetSpinners', function(message) {
  $('.shiny-spinner-output-container[data-spin-once]').each(function() {
    if ($(this).find('[id^="' + message.prefix + '"]').length > 0) {
      $(this).removeClass('spinner-rendered');
    }
  });
});

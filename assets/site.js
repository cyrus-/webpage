// Bushel cross-references ([text](:slug)) are rendered by arod as
// <side-note data-link=... data-title=...>text</side-note>. Anil's site
// script turns these into hover previews; here they just become links.
(function () {
  document.querySelectorAll("side-note[data-link]").forEach(function (n) {
    var a = document.createElement("a");
    a.href = n.getAttribute("data-link");
    var t = n.getAttribute("data-title");
    if (t) a.title = t;
    a.className = "xref";
    while (n.firstChild) a.appendChild(n.firstChild);
    n.replaceWith(a);
  });
})();

// Tab bar: mark the tab for the current section. Paths may carry a prefix
// (e.g. /~comar), so match on the tab's own path suffix.
(function () {
  var path = location.pathname.replace(/\/index\.html$/, "/").replace(/\/+$/, "");
  var tabs = Array.prototype.slice.call(document.querySelectorAll(".tab-bar .tab"));
  var root = tabs.length ? tabs[0].pathname.replace(/\/+$/, "") : "";
  var section = { "/projects": "/", "/videos": "/talks", "/ideas": "/" };
  var best = null;
  tabs.forEach(function (t) {
    var tp = t.pathname.replace(/\/+$/, "");
    if (t === tabs[0]) {
      if (path === tp) best = t;
      return;
    }
    if (path === tp || path.indexOf(tp + "/") === 0) best = t;
  });
  if (!best) {
    Object.keys(section).forEach(function (k) {
      if (path.indexOf(root + k) === 0) {
        tabs.forEach(function (t) {
          if (t.pathname.replace(/\/+$/, "") === (root + section[k]).replace(/\/+$/, "")) best = t;
        });
      }
    });
  }
  if (best) best.classList.add("active");
})();

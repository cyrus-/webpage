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

// Design preview: add #design to any URL to get a switcher for the home
// page layout, the card style and the background colour. Choices are
// remembered (localStorage) while you browse, and only apply in design
// mode. (A hash rather than a query string, since the preview server 404s
// on "/?..." for the home page.)
(function () {
  if (!/design/.test(location.hash)) return;
  var root = document.documentElement;
  function get(k) { try { return localStorage.getItem(k); } catch (e) { return null; } }
  function set(k, v) { try { localStorage.setItem(k, v); } catch (e) {} }
  var groups = [
    { label: "layout", attr: "data-layout", key: "layout", options: [
      ["default", "stack"], ["columns", "2 columns"], ["grid", "grid"], ["three", "3 columns"] ] },
    { label: "cards", attr: "data-cards", key: "cards", options: [
      ["default", "tan shadow"], ["index", "index card"], ["flat", "flat"] ] },
  ];
  var bar = document.createElement("div");
  bar.className = "design-switcher";
  groups.forEach(function (g) {
    function apply(v) {
      if (v && v !== "default") root.setAttribute(g.attr, v); else root.removeAttribute(g.attr);
    }
    apply(get(g.key));
    var row = document.createElement("div");
    row.className = "row";
    var lab = document.createElement("span");
    lab.textContent = g.label;
    row.appendChild(lab);
    g.options.forEach(function (o) {
      var b = document.createElement("button");
      b.textContent = o[1];
      b.setAttribute("aria-pressed", String((get(g.key) || "default") === o[0]));
      b.onclick = function () {
        set(g.key, o[0]);
        apply(o[0]);
        row.querySelectorAll("button").forEach(function (x) {
          x.setAttribute("aria-pressed", String(x === b));
        });
      };
      row.appendChild(b);
    });
    bar.appendChild(row);
  });

  // background colour picker
  function current() { return getComputedStyle(root).getPropertyValue("--paper").trim(); }
  var row = document.createElement("div");
  row.className = "row";
  var lab = document.createElement("span");
  lab.textContent = "background";
  var input = document.createElement("input");
  input.type = "color";
  var hex = document.createElement("code");
  function show(v) { input.value = v; hex.textContent = v; }
  function setPaper(v) { root.style.setProperty("--paper", v); show(v); set("paperColor", v); }
  input.oninput = function () { setPaper(input.value); };
  var reset = document.createElement("button");
  reset.className = "reset";
  reset.textContent = "reset";
  reset.onclick = function () {
    root.style.removeProperty("--paper");
    try { localStorage.removeItem("paperColor"); } catch (e) {}
    show(current());
  };
  [lab, input, hex, reset].forEach(function (el) { row.appendChild(el); });
  bar.appendChild(row);
  document.body.appendChild(bar);
  if (get("paperColor")) setPaper(get("paperColor")); else show(current());

  // keep #design on internal links while previewing
  document.querySelectorAll("a[href^='/']").forEach(function (a) {
    if (a.href.indexOf("#") < 0) a.href += "#design";
  });
})();

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

// Design preview: add #design to any URL to get a background colour picker.
// (A hash rather than a query string, since the preview server 404s on
// "/?..." for the home page.) The picked colour is remembered
// (localStorage) while you browse, and only applies in design mode.
(function () {
  if (!/design/.test(location.hash) && !/[?&]design\b/.test(location.search)) return;
  var root = document.documentElement;
  function get(k) { try { return localStorage.getItem(k); } catch (e) { return null; } }
  function set(k, v) { try { localStorage.setItem(k, v); } catch (e) {} }
  function current() { return getComputedStyle(root).getPropertyValue("--paper").trim(); }

  var bar = document.createElement("div");
  bar.className = "design-switcher";
  var label = document.createElement("span");
  label.textContent = "background";
  var input = document.createElement("input");
  input.type = "color";
  var hex = document.createElement("code");
  function show(v) { input.value = v; hex.textContent = v; }
  function setPaper(v) { root.style.setProperty("--paper", v); show(v); set("paperColor", v); }
  input.oninput = function () { setPaper(input.value); };
  var reset = document.createElement("button");
  reset.textContent = "reset";
  reset.onclick = function () {
    root.style.removeProperty("--paper");
    try { localStorage.removeItem("paperColor"); } catch (e) {}
    show(current());
  };
  [label, input, hex, reset].forEach(function (el) { bar.appendChild(el); });
  document.body.appendChild(bar);
  if (get("paperColor")) setPaper(get("paperColor")); else show(current());

  // keep #design on internal links while previewing
  document.querySelectorAll("a[href^='/']").forEach(function (a) {
    if (a.href.indexOf("#") < 0) a.href += "#design";
  });
})();

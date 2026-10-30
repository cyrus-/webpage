// Design preview: add ?design=1 to any URL to get a background colour
// picker. The picked colour is remembered (localStorage) while you browse,
// and only applies when ?design=1 is present. Not shown on the real site.
(function () {
  if (!/[?&]design\b/.test(location.search)) return;
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

  // keep ?design=1 on internal links while previewing
  document.querySelectorAll("a[href^='/']").forEach(function (a) {
    if (!/[?&]design\b/.test(a.href)) a.href += (a.href.indexOf("?") < 0 ? "?" : "&") + "design=1";
  });
})();

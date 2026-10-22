# APPEND this entire block to the END of src/review_vad.R.
# It replaces the earlier theme helper, not your data/import functions.

.review_vad_theme <- function(p) {
  htmlwidgets::onRender(p, '
function(el) {
  if (el.__dreamVadCleanup) el.__dreamVadCleanup();
  var timer, alive = true, lastKey = "", busy = false;
  var palettes = {
    lavender: {point: "#B18AF0", grid: "#C0ADD9", text: "#514365", muted: "#88759F"},
    ocean: {point: "#4BAFCB", grid: "#91BDCB", text: "#284F60", muted: "#608591"},
    forest: {point: "#76AE8A", grid: "#A0BEA9", text: "#354F40", muted: "#6B8976"}
  };
  function themeName() {
    var selects = document.querySelectorAll("select");
    for (var i = 0; i < selects.length; i++) {
      var all = Array.from(selects[i].options).map(function(o) {
        return (o.text + " " + o.value).toLowerCase();
      }).join(" ");
      if (all.includes("lavender") && all.includes("ocean") && all.includes("forest")) {
        var option = selects[i].options[selects[i].selectedIndex];
        var selected = option ? (option.text + " " + option.value).toLowerCase() : "";
        if (selected.includes("ocean")) return "ocean";
        if (selected.includes("forest")) return "forest";
        return "lavender";
      }
    }
    return "lavender";
  }
  function cardBackground() {
    // Find the actual enclosing card surface, rather than the plot canvas.
    var card = el.closest(".card, .bslib-card");
    var node = card ? (card.querySelector(".card-body") || card) : el.parentElement;
    while (node) {
      var color = getComputedStyle(node).backgroundColor;
      if (color && color !== "transparent" && color !== "rgba(0, 0, 0, 0)") return color;
      node = node.parentElement;
    }
    return "rgb(250, 249, 253)";
  }
  function apply() {
    if (!alive || !el.isConnected || !window.Plotly || busy) return;
    var name = themeName(), palette = palettes[name];
    var bg = cardBackground();
    var family = getComputedStyle(document.body).fontFamily;
    var key = name + bg + family;
    if (key === lastKey) return;
    busy = true;
    var update = {
      "paper_bgcolor": bg,
      "plot_bgcolor": bg,
      "scene.bgcolor": bg,
      "font.family": family,
      "font.color": palette.text,
      "hoverlabel.bgcolor": bg,
      "hoverlabel.bordercolor": palette.point,
      "hoverlabel.font.color": palette.text,
      "margin.l": 45, "margin.r": 45,
      "margin.b": 55, "margin.t": 20
    };
    ["xaxis", "yaxis", "zaxis"].forEach(function(axis) {
      var prefix = "scene." + axis + ".";
      update[prefix + "showgrid"] = true;
      // Solid colours avoid very faint alpha-blended lines in WebGL.
      update[prefix + "gridcolor"] = palette.grid;
      update[prefix + "gridwidth"] = 2;
      update[prefix + "showbackground"] = true;
      // All three planes use exactly the enclosing card colour.
      update[prefix + "backgroundcolor"] = bg;
      update[prefix + "showline"] = true;
      update[prefix + "linecolor"] = palette.grid;
      update[prefix + "linewidth"] = 1;
      update[prefix + "zeroline"] = false;
      update[prefix + "tickfont.color"] = palette.muted;
      update[prefix + "tickfont.family"] = family;
      update[prefix + "title.font.color"] = palette.text;
      update[prefix + "title.font.family"] = family;
    });
    Promise.resolve(window.Plotly.relayout(el, update))
      .then(function() {
        return window.Plotly.restyle(el, {"marker.color": palette.point}, [0]);
      })
      .then(function() { lastKey = key; })
      .catch(function(error) { console.warn("VAD appearance update:", error); })
      .finally(function() { busy = false; });
  }
  function schedule() {
    clearTimeout(timer);
    timer = setTimeout(apply, 180);
  }
  document.addEventListener("change", schedule);
  if (window.jQuery) window.jQuery(document).on("shiny:inputchanged.dreamVad", schedule);
  var observer = new MutationObserver(schedule);
  observer.observe(document.head, {childList: true, subtree: true, characterData: true});
  observer.observe(document.documentElement, {attributes: true});
  observer.observe(document.body, {attributes: true});
  var card = el.closest(".card, .bslib-card");
  if (card) observer.observe(card, {attributes: true});
  el.__dreamVadCleanup = function() {
    alive = false;
    clearTimeout(timer);
    observer.disconnect();
    document.removeEventListener("change", schedule);
    if (window.jQuery) window.jQuery(document).off("shiny:inputchanged.dreamVad", schedule);
  };
  apply();
}
')
}

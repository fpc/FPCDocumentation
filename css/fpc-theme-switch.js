/*
    fpc-theme-switch.js

    Adds two small menus to the Free Pascal HTML documentation: one to pick
    the page style and one to pick light or dark colours. The choice is kept
    in the storage of the browser, so it also holds for the next page.

    The file is loaded from the head of the page, directly after the
    stylesheet link, so the chosen style is in place before the page is drawn.

    This file is free software, distributed under the same terms as the
    Free Pascal documentation itself.
*/

(function () {
  "use strict";

  var LINK_ID = "fpc-doc-style-link";
  var STYLE_KEY = "fpc-doc-style";
  var MODE_KEY = "fpc-doc-mode";

  var STYLES = [
    ["fpc-modern.css", "Modern"],
    ["fpc-book.css", "Book"],
    ["fpc-compact.css", "Compact"]
  ];

  var MODES = [
    ["auto", "Auto"],
    ["light", "Light"],
    ["dark", "Dark"]
  ];

  // Some browsers refuse the storage when the page is read from disk.
  function readSetting(aKey) {
    try {
      return window.localStorage.getItem(aKey);
    } catch (lError) {
      return null;
    }
  }

  function writeSetting(aKey, aValue) {
    try {
      window.localStorage.setItem(aKey, aValue);
    } catch (lError) {
      /* The choice is then simply not remembered. */
    }
  }

  // Returns the value only when it is one of the names in the list.
  function knownValue(aList, aValue) {
    var i;
    for (i = 0; i < aList.length; i++) {
      if (aList[i][0] === aValue) {
        return aValue;
      }
    }
    return null;
  }

  function styleLink() {
    return document.getElementById(LINK_ID);
  }

  function currentStyle() {
    var lLink = styleLink();
    var lHref = lLink ? (lLink.getAttribute("href") || "") : "";
    return lHref.substring(lHref.lastIndexOf("/") + 1);
  }

  // Points the stylesheet link at another file, keeping the directory it is in.
  function applyStyle(aStyle) {
    var lLink = styleLink();
    var lHref, lSlash;
    if (!lLink) {
      return;
    }
    lHref = lLink.getAttribute("href") || "";
    lSlash = lHref.lastIndexOf("/");
    lLink.setAttribute("href",
      lSlash < 0 ? aStyle : lHref.substring(0, lSlash + 1) + aStyle);
  }

  // Marks the page as light or dark, or lets the screen setting decide.
  function applyMode(aMode) {
    if (aMode === "light" || aMode === "dark") {
      document.documentElement.setAttribute("data-theme", aMode);
    } else {
      document.documentElement.removeAttribute("data-theme");
    }
  }

  function makeMenu(aList, aCurrent, aLabel, aOnChange) {
    var lSelect = document.createElement("select");
    var lOption, i;
    lSelect.className = "fpc-switch-select";
    lSelect.setAttribute("aria-label", aLabel);
    lSelect.title = aLabel;
    for (i = 0; i < aList.length; i++) {
      lOption = document.createElement("option");
      lOption.value = aList[i][0];
      lOption.appendChild(document.createTextNode(aList[i][1]));
      if (aList[i][0] === aCurrent) {
        lOption.selected = true;
      }
      lSelect.appendChild(lOption);
    }
    lSelect.onchange = function () {
      aOnChange(lSelect.value);
    };
    return lSelect;
  }

  // Puts the menus in the bar at the top, or in a bar of their own.
  function placeMenus(aBox) {
    var lBar = document.querySelector("div.crosslinks p");
    if (lBar) {
      lBar.appendChild(aBox);
      return;
    }
    lBar = document.createElement("div");
    lBar.className = "fpc-switch-bar";
    lBar.appendChild(aBox);
    document.body.insertBefore(lBar, document.body.firstChild);
  }

  function buildMenus() {
    var lBox;
    if (!styleLink() || !document.body) {
      return;
    }
    lBox = document.createElement("div");
    lBox.className = "fpc-switch";
    lBox.appendChild(makeMenu(STYLES, currentStyle(), "Page style",
      function (aValue) {
        applyStyle(aValue);
        writeSetting(STYLE_KEY, aValue);
      }));
    lBox.appendChild(makeMenu(MODES, readSetting(MODE_KEY) || "auto", "Light or dark",
      function (aValue) {
        applyMode(aValue);
        writeSetting(MODE_KEY, aValue);
      }));
    placeMenus(lBox);
  }

  var lSavedStyle = knownValue(STYLES, readSetting(STYLE_KEY));
  var lSavedMode = knownValue(MODES, readSetting(MODE_KEY));

  if (lSavedStyle) {
    applyStyle(lSavedStyle);
  }
  if (lSavedMode) {
    applyMode(lSavedMode);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", buildMenus);
  } else {
    buildMenus();
  }
}());

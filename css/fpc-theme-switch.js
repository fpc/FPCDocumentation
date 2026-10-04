/*
    fpc-theme-switch.js

    Adds to every page of the Free Pascal HTML documentation:
      - a menu to pick the page style,
      - a menu to pick light or dark colours,
      - a sidebar with the chapters of the manual, with the sections of the
        chapter being read folded open underneath it.
    The choices are kept in the storage of the browser, so they also hold for
    the next page.

    The file is loaded from the head of the page, directly after the
    stylesheet link, so the chosen style is in place before the page is drawn.
    The contents of the sidebar come from fpc-toc.js, which the makefile
    writes next to the pages. Both files are left out of the chm help files.

    This file is free software, distributed under the same terms as the
    Free Pascal documentation itself.
*/

(function () {
  "use strict";

  var LINK_ID = "fpc-doc-style-link";
  var STYLE_KEY = "fpc-doc-style";
  var MODE_KEY = "fpc-doc-mode";
  var SIDEBAR_KEY = "fpc-doc-sidebar";
  var SIDEBAR_CLASS = "fpc-sidebar-open";

  /* Below this width the sidebar lies over the text, so it starts closed. */
  var WIDE_SCREEN = 60 * 16;

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

  /* Kinds of entry in the contents list of a manual. */
  var CHAPTER_KINDS = ["partToc", "chapterToc", "likechapterToc", "appendixToc", "lotToc"];
  var SECTION_KINDS = ["sectionToc", "likesectionToc"];

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

  function haveToc() {
    return typeof window.fpcDocToc === "string" && window.fpcDocToc !== "";
  }

  function sidebarOpen() {
    return document.documentElement.className.indexOf(SIDEBAR_CLASS) >= 0;
  }

  function showSidebar(aOpen) {
    var lRoot = document.documentElement;
    if (aOpen) {
      if (!sidebarOpen()) {
        lRoot.className = (lRoot.className + " " + SIDEBAR_CLASS).replace(/^\s+/, "");
      }
    } else {
      lRoot.className = lRoot.className.replace(SIDEBAR_CLASS, "").replace(/\s+/g, " ");
    }
  }

  // The file name of a page, without the directory and without the anchor.
  function pageName(aHref) {
    var lPath = aHref.split("#")[0];
    return lPath.substring(lPath.lastIndexOf("/") + 1);
  }

  function entryKind(aName) {
    if (CHAPTER_KINDS.indexOf(aName) >= 0) {
      return "chapter";
    }
    if (SECTION_KINDS.indexOf(aName) >= 0) {
      return "section";
    }
    return aName.substring(aName.length - 3) === "Toc" ? "deeper" : "";
  }

  // The number in front of an entry, which is the text before its link.
  function entryNumber(aSpan) {
    var lText = "";
    var lNode = aSpan.firstChild;
    while (lNode && lNode.nodeName.toLowerCase() !== "a") {
      if (lNode.nodeType === 3) {
        lText += lNode.nodeValue;
      }
      lNode = lNode.nextSibling;
    }
    return lText.replace(/\s+/g, " ").replace(/^\s+|\s+$/g, "");
  }

  // Turns the contents list of the manual into a plain list of entries.
  function readToc() {
    var lHolder = document.createElement("div");
    var lSpans, lSpan, lLink, lKind, lList, i;
    lHolder.innerHTML = window.fpcDocToc;
    lSpans = lHolder.getElementsByTagName("span");
    lList = [];
    for (i = 0; i < lSpans.length; i++) {
      lSpan = lSpans[i];
      lKind = entryKind(lSpan.className || "");
      if (!lKind) {
        continue;
      }
      lLink = lSpan.getElementsByTagName("a")[0];
      if (!lLink) {
        continue;
      }
      lList.push({
        kind: lKind,
        href: lLink.getAttribute("href") || "",
        file: pageName(lLink.getAttribute("href") || ""),
        number: entryNumber(lSpan),
        title: lLink.textContent || lLink.innerText || ""
      });
    }
    return lList;
  }

  function makeEntry(aEntry, aHere) {
    var lBox = document.createElement("div");
    var lLink = document.createElement("a");
    var lNumber;
    lBox.className = "fpc-toc-entry fpc-" + aEntry.kind + (aHere ? " fpc-here" : "");
    lLink.setAttribute("href", aEntry.href);
    if (aEntry.number) {
      lNumber = document.createElement("span");
      lNumber.className = "fpc-num";
      lNumber.appendChild(document.createTextNode(aEntry.number));
      lLink.appendChild(lNumber);
    }
    lLink.appendChild(document.createTextNode(aEntry.title));
    lBox.appendChild(lLink);
    return lBox;
  }

  /*
    Shows every chapter, and under the chapter being read its sections. When
    the page being read is deeper than a section, its section is marked.
  */
  function buildSidebar() {
    var lEntries = readToc();
    var lHere = pageName(window.location.pathname);
    var lNav, lHead, lChapter, lSection, lMark, lMarked, i;

    lChapter = -1;
    lSection = -1;
    lMark = -1;
    for (i = 0; i < lEntries.length; i++) {
      if (lEntries[i].kind === "chapter") {
        lChapter = i;
        lSection = -1;
      } else if (lEntries[i].kind === "section") {
        lSection = i;
      }
      if (lEntries[i].file === lHere) {
        lMark = (lEntries[i].kind === "deeper")
          ? (lSection >= 0 ? lSection : lChapter)
          : i;
        break;
      }
    }
    /* The chapter the marked entry belongs to, whose sections are shown. */
    lChapter = -1;
    for (i = 0; i <= lMark && i < lEntries.length; i++) {
      if (lEntries[i].kind === "chapter") {
        lChapter = i;
      }
    }

    lNav = document.createElement("nav");
    lNav.className = "fpc-sidebar";
    lHead = document.createElement("div");
    lHead.className = "fpc-sidebar-head";
    lHead.appendChild(document.createTextNode("Contents"));
    lNav.appendChild(lHead);

    lSection = -1;
    for (i = 0; i < lEntries.length; i++) {
      if (lEntries[i].kind === "chapter") {
        lSection = i;
      }
      if (lEntries[i].kind === "chapter"
          || (lEntries[i].kind === "section" && lSection === lChapter)) {
        lNav.appendChild(makeEntry(lEntries[i], i === lMark));
      }
    }

    /* On a narrow screen the sidebar covers the text, so a click closes it. */
    lNav.onclick = function (aEvent) {
      var lTarget = aEvent.target || aEvent.srcElement;
      if (lTarget && lTarget.nodeName.toLowerCase() === "a"
          && window.innerWidth < WIDE_SCREEN) {
        showSidebar(false);
      }
    };

    document.body.insertBefore(lNav, document.body.firstChild);

    lMarked = lNav.getElementsByClassName("fpc-here")[0];
    if (lMarked) {
      lNav.scrollTop = lMarked.offsetTop - (lNav.clientHeight / 2);
    }
  }

  function makeToggle() {
    var lButton = document.createElement("button");
    lButton.type = "button";
    lButton.className = "fpc-switch-toggle";
    lButton.setAttribute("aria-label", "Show or hide the contents");
    lButton.title = "Show or hide the contents";
    lButton.appendChild(document.createTextNode("Contents"));
    lButton.onclick = function () {
      var lOpen = !sidebarOpen();
      showSidebar(lOpen);
      writeSetting(SIDEBAR_KEY, lOpen ? "open" : "closed");
    };
    return lButton;
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
    if (haveToc()) {
      lBox.appendChild(makeToggle());
    }
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

  function build() {
    if (!document.body) {
      return;
    }
    if (haveToc()) {
      buildSidebar();
    }
    buildMenus();
  }

  var lSavedStyle = knownValue(STYLES, readSetting(STYLE_KEY));
  var lSavedMode = knownValue(MODES, readSetting(MODE_KEY));
  var lSavedSidebar = readSetting(SIDEBAR_KEY);

  if (lSavedStyle) {
    applyStyle(lSavedStyle);
  }
  if (lSavedMode) {
    applyMode(lSavedMode);
  }
  /* Done before the page is drawn, so the text does not shift afterwards. */
  if (haveToc()) {
    showSidebar(lSavedSidebar === "open"
      || (lSavedSidebar !== "closed" && window.innerWidth >= WIDE_SCREEN));
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", build);
  } else {
    build();
  }
}());

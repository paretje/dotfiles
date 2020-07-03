// ==UserScript==
// @name         Use Invidious
// @version      0.0.1
// @description  Redirect page to Invidious
// @author       Paretje
// @match        *://www.youtube.com/*
// @run-at       document-start
// @grant        none
// ==/UserScript==

(function() {
    'use strict';
    window.location.replace("https://yewtu.be" + window.location.pathname + window.location.search);
})();

// ==UserScript==
// @name         Use Invidious
// @version      0.0.1
// @description  Redirect page to Invidious
// @author       Paretje
// @match        *://www.youtube.com/*
// @match        *://yewtu.be/*
// @match        *://invidious.13ad.de/*
// @run-at       document-start
// @grant        none
// ==/UserScript==

(function() {
    'use strict';
    window.location.replace("https://invidious.fdn.fr" + window.location.pathname + window.location.search);
})();

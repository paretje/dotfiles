// ==UserScript==
// @name         Use Youtube
// @version      0.0.1
// @description  Redirect page to Invidious
// @author       Paretje
// @match        *://invidio.us/*
// @match        *://invidious.snopyta.org/*
// @match        *://yewtu.be/*
// @match        *://invidious.13ad.de/*
// @match        *://invidious.fdn.fr/*
// @run-at       document-start
// @grant        none
// ==/UserScript==

(function() {
    'use strict';
    window.location.replace("https://youtube.com" + window.location.pathname + window.location.search);
})();

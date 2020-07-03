// ==UserScript==
// @name         Use Nitter
// @version      0.0.1
// @description  Redirect page to Nitter
// @author       Paretje
// @match        *://twitter.com/*
// @run-at       document-start
// @grant        none
// ==/UserScript==

(function() {
    'use strict';
    window.location.replace("https://nitter.net" + window.location.pathname + window.location.search);
})();

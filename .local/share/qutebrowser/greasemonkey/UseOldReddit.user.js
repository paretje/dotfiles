// ==UserScript==
// @name         Use Old Reddit
// @version      0.0.1
// @description  Redirect page to Old Reddit
// @author       Paretje
// @match        *://www.reddit.com/*
// @run-at       document-start
// @grant        none
// ==/UserScript==

(function() {
    'use strict';
    window.location.replace("https://old.reddit.com" + window.location.pathname + window.location.search);
})();

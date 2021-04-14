// ==UserScript==
// @name         Use International PostNL Tracking
// @version      0.0.1
// @description  Redirect page to the more detailed international tracking page of PostNL
// @author       Paretje
// @match        *://jouw.postnl.nl/track-and-trace/*
// @match        *://jouw.postnl.be/track-and-trace/*
// @run-at       document-start
// @grant        none
// ==/UserScript==

(function() {
    'use strict';
    const url_regex = /^\/track-and-trace\/(?<code>[0-9A-Z]+)-(?<country>[A-Z]+)-(?<zip>[0-9]+)$/;
    const matches = window.location.pathname.match(url_regex);
    if (matches) {
      window.location.replace("https://internationalparceltracking.com/Main.aspx#/track/" + matches.groups.code + "/" + matches.groups.country + "/" + matches.groups.zip);
    }
})();

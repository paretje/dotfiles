// ==UserScript==
// @name         Microsoft Teams Notifications
// @version      0.0.1
// @description  Enables 
// @author       Paretje
// @match        *://teams.microsoft.com/*
// @run-at       document-idle
// @grant        none
// ==/UserScript==
(function() {
    'use strict';

    var unfocussed_chat = false;

    function getCalendarButton() {
        let calendar_span = document.querySelector('span[data-tid="appBarText-Calendar"]');
        if (calendar_span) {
            return calendar_span.parentNode;
        }
    }

    function getChatButton() {
        let chat_span = document.querySelector('span[data-tid="appBarText-Chat"]');
        if (chat_span) {
            return chat_span.parentNode;
        }
    }

    function lostFocus() {
        let chat_button = getChatButton();
        if (chat_button && chat_button.classList.contains("app-bar-selected")) {
            let calendar_button = getCalendarButton();
            if (calendar_button) {
                calendar_button.click();
                unfocussed_chat = true;
            }
        }
    }

    function gotFocus() {
        if (!unfocussed_chat) {
            return;
        }
        let chat_button = getChatButton();
        if (chat_button) {
            chat_button.click();
            unfocussed_chat = false;
        }
    }

    function periodicFocusCheck() {
        setInterval(function() {
            // document.hidden is false when the window is simply on another workspace, so unreliable
            if (!document.hasFocus()) {
                if (!unfocussed_chat) {
                    lostFocus();
                }
            } else if (unfocussed_chat) {
                gotFocus();
            }
        }, 250);
    }

    function registerListeners() {
        window.addEventListener('focus', gotFocus);
        window.addEventListener('blur', lostFocus);
    }

    // periodicFocusCheck();
    registerListeners();
})();

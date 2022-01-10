/*global CookieControl */

"use strict";

const COOKIECONTROL_SRC = "https://cc.cdn.civiccomputing.com/9/cookieControl-9.4.min.js";

var config = {
    apiKey: '0144bb529edfc11085639c504ecae11c033749c5',
    product: 'PRO_MULTISITE',
    iabCMP: true,
    iabConfig: {
        fullLegalDescriptions: false,
        saveOnlyOnClose: true
    },
        
    necessaryCookies: [
        "AKA_Bypass", // Akamai
        "MOWxPrefs", // WAVE and TIDE
        "MOWxUnits", // WAVE and TIDE
        "_ga_K77XZ36FRH", // Google Analytics
        "_ga", // Google Analytics
        "_gat_UA-51679231-1", // Google Analytics
        "_gid", // Google Analytics
        "AMAuthCookie", // Meto
        "amlbcookie", // Meto
        "JSESSIONID", // Servlet container Session cookie
        "WT_FPC", // Meto
        "authpath", // Meto
        "DProPCookie", // Meto
        "iPlanetDirectoryPro", // Meto
        "Meto-LocationGroup", // Meto
        "Meto-Subscription", // Meto
        "Meto-UserAccount" // Meto
    ],
    sameSiteCookie: false,
    encodeCookie: true,
    rejectButton: true,
    closeStyle: "button",
    layout: "slideout",
    consentCookieExpiry: 365,
    subDomains: false,
    position: 'left',
    setInnerHTML: true,
    branding: {
        fontFamily: "FSEmeric, Arial, san-serif",
        fontSize: "14px",
        fontSizeTitle: "1.4em",
        fontSizeIntro: "1.2em",
        fontSizeHeaders: "1.1em",
        backgroundColor: '#262626',
        acceptText: "#262626",
        acceptBackground: "#b9db0c",
        toggleText: '#262626',
        toggleColor: '#262626',
        toggleBackground: '#f5f5f5',
        removeIcon: true,
        removeAbout: true
    },
    accessibility: {
        outline: true
    },
    text: {
        closeLabel: "Save Preferences and Exit",
        iabCMP: {
            panelTitle: "This website uses cookies",
            panelIntro1: "The Met Office website uses necessary cookies to make our site work. We also use optional cookies to help us improve your experience, understand how the site is being used for future improvements, and serve personalised advertising.",
            panelIntro2: "You can review the third-party vendors and how this information is being used below. You can also customise your consent preferences for cookies and identifying technologies.",
            panelIntro3: "To agree to the use of these technologies select ‘Accept all’. You can change your preferences at any time by visiting our <a href='//www.metoffice.gov.uk/about-us/legal/cookies'>cookie policy</a>.",
            aboutIab: "Personalised advertising adheres to the",
            iabName: "IAB Transparency and Consent Framework (TCF).",
            iabLink: "https://iabeurope.eu/iab-europe-transparency-consent-framework-policies/"
        }
    }
};

document.addEventListener('DOMContentLoaded', (event) => {
    // add the link to bring up the banner
    var changeConsentLinks = document.querySelectorAll("a.change-consent");
    Array.prototype.forEach.call(changeConsentLinks, function (link, i) {
        link.href = "javascript:CookieControl.open()";
    });
});

// if Cookie Control script is not loaded, then load it here
if (typeof CookieControl !== "object"){
    let script = document.createElement('script');
    script.onload = function () {
        CookieControl.load(config);
    };
    script.src = COOKIECONTROL_SRC;

    document.head.appendChild(script);
} else {
    // Cookie Control is already loaded
    CookieControl.load(config);
}



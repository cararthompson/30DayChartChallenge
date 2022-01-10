"use strict";

(function loadAdhocScripts() {
    const logsEnabled = window.location.href.indexOf('adhoc=debug') !== -1;

    const scripts = [
        {
            url: "/public/adhoc/covid-19-homepage-banner-message.js",
            enabled: true,
            pathnameMatch: [
                // matches only:
                // home page
                { type: 'exact', pathnames: ["/"] },

                // matches only pages starting with /weather:
                // /weather/warnings-and-advice/uk-warnings
                // /weather/warnings-and-advice/seasonal-advice
                { type: 'like', pathnames: ["^/weather"] },


                // matches warnings only:
                // /weather/warnings-and-advice/uk-warnings
                { type: 'like', pathnames: ["/weather/warnings-and-advice/uk-warnings"] },

            ],
            hostExactMatch: [
                "localhost",
                "test.metoffice.gov.uk",
                "acct.metoffice.gov.uk"
            ]
        },
        {
            url: "/public/adhoc/covid-19-seasonal-advice-banner-message.js",
            enabled: false
        },
        {
            url: "/public/adhoc/covid-19-mountain-banner-message.js",
            enabled: false
        },
        {
            url: "/public/adhoc/data-outage-banner-message.js",
            enabled: false
        },
    ];

    function loadScript(url) {
        let script = document.createElement("script");
        script.src = url;
        script.setAttribute("defer", "defer");
        document.head.appendChild(script);
    }
    function hostMatch(adhoc) {
        if (!adhoc.hostExactMatch || adhoc.hostExactMatch.length === 0)
            return false;
        return adhoc.hostExactMatch.indexOf(window.location.host) !== -1;
    }
    function pathnameMatch(adhoc) {
        if (!adhoc.pathnameMatch || adhoc.pathnameMatch.length === 0)
            return false;
        const pathname = window.location.pathname;
        for (const matchRule of adhoc.pathnameMatch) {
            if (!matchRule.pathnames)
                continue;

            switch (matchRule.type) {
                case 'exact':
                    if (matchRule.pathnames.indexOf(pathname) !== -1) {
                        logsEnabled && console.log('loadAdhocScripts:exact pathname match', matchRule, adhoc.url);
                        return true;
                    }
                    break;
                case 'like':
                    for (const regex of matchRule.pathnames) {
                        let re = new RegExp(regex);
                        if (re.test(pathname)) {
                            logsEnabled && console.log('loadAdhocScripts:like pathname match', regex, adhoc.url);
                            return true;
                        }
                    }
                    break;
            };
        }

        return false;
    }
    for (const adhocScript of scripts) {
        if (!adhocScript.enabled)
            continue;
        if (!hostMatch(adhocScript)) {
            logsEnabled && console.log('loadAdhocScripts:host does not match', adhocScript);
            continue;
        }
        if (!pathnameMatch(adhocScript)) {
            logsEnabled && console.log('loadAdhocScripts:pathname does not match', adhocScript);
            continue;
        }
        logsEnabled && console.log('loadAdhocScripts: loading script', adhocScript.url);
        loadScript(adhocScript.url);
    }


})();



window.metoffice = window.metoffice || {};
window.metoffice.loadScriptForSomeUsers = function(id, url, percentageOfUsers, timeoutDays) {

    const logsEnabled = window.location.href.indexOf('adhoc=debug') !== -1;

    const getCookie = function(name) {
        const value = `; ${document.cookie}`;
        const parts = value.split(`; ${name}=`);
        
        return (parts.length === 2)?parts.pop().split(';').shift():null;
    }

    const loadScript = function(url){
        let script = document.createElement("script");
        script.src = url;
        script.setAttribute("defer", "defer");
        document.head.appendChild(script);
    }

    let scriptActive = getCookie(`mo-adhoc-${id}`)
    if (scriptActive === null) {
        scriptActive = (Math.random() * 100 < percentageOfUsers) ? "true" : "false";
        let now = new Date();
        now.setTime(now.getTime() + 1000 * 60 * 60 * 24 * parseInt(timeoutDays)); 
        document.cookie = `mo-adhoc-${id}=${scriptActive}; path=/;expires=${now.toUTCString()}`;
    }

    if (scriptActive === 'true') {
        logsEnabled && console.log('loadScriptForSomeUsers: loading script', adhocScript.url);
        loadScript(url);
    } else {
        logsEnabled && console.log('loadScriptForSomeUsers: did not select script to load', adhocScript.url);
    }
}

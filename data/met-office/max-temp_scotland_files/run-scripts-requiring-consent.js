/*
* Delays running of scripts, until consent or denial has been given.
*
* A script can be conditioned to run by changing the following.
* - data-run-after-consent="" attribute added
* - type attribute value changed to "text/plain"  -to prevent the script from running normally
* - src attribute changed to be data-src, for external scripts. 
* - async / defer attributes removed.
* This will allow the HTML code to pass the HTML standards validation.
*
* By default, a script will run only run if consent purpose 1 is given.  This can be overridden by
* by providing a data-consent-purposes attribute containing a comma separated list of:
* - purposes that need to be consented to by the user,
* - purposes that need to have no consent from the user.
*
* For example: the following will output to the console only if consent purposes 1,5 and 6 are granted,
* and consent purpose 7 is not granted.
* 
* <script type="text/plain" data-run-after-consent="" data-consent-purposes="1,5,6,!7">
*   console.log('hello world');
* </script>
*
* By default, all listed consents or denials in the data-consent-purposes list are required in order
* for the script to run.  In other words, values are "ANDed" together. If instead, the intention is to
* run if any one of the consents or denials are present ("ORed" together), the attribute:
* - data-consent-boolean-op="or"
* may be added.
*
*  data-consent-purposes="*" means the script will always run after the CMP has been loaded. Downstream scripts are
* expected to handle consent appropriately.
* 
*/

window.metoffice = window.metoffice || {};
window.metoffice.runConsentScript = {     
    consentPurposes: null,           
    waitForConsent: function() {

        // If TCF not yet available, return and try again in 100ms.
        if (typeof window.__tcfapi == 'undefined') {
            setTimeout(window.metoffice.runConsentScript.waitForConsent,100);
            return;
        }

        __tcfapi("addEventListener",2,function(evt,success){
            if (!success || evt.eventStatus !== "useractioncomplete"){
                return;
                }                
            
            window.metoffice.runConsentScript.consentPurposes = evt.purpose.consents;
            window.metoffice.runConsentScript.findAndRunScript();
        });    
    },
    findAndRunScript: function() {
        let scriptList = document.querySelectorAll("script[data-run-after-consent]");  
        let script = null;
        
        // Find first script that matches consent given.
        outerLoop: 
        for (let i = 0; i < scriptList.length; i++){
            script = scriptList[i];         
            let consentPurposes = ["1"]; // default consent if not specified
            let consentBooleanOp; // "and" by default
            if (typeof script.dataset.consentPurposes === "string") { // an empty string is acceptable
                consentPurposes = script.dataset.consentPurposes.split(",");
            }
            if (typeof script.dataset.consentBooleanOp === "string") {
                consentBooleanOp = script.dataset.consentBooleanOp;
            }
            for (let j = 0; j < consentPurposes.length; j++) {
                if (consentPurposes[j] !== "*") { // wildcard - always run this script
                    if (consentPurposes[j].startsWith('!')) {
                        let consentPurposeNbr = parseInt(consentPurposes[j].substring(1));
                        if (consentBooleanOp === "or") {
                            if (!window.metoffice.runConsentScript.consentPurposes[consentPurposeNbr]) {
                                window.metoffice.runConsentScript.executeScript(script);
                                break outerLoop;
                            }
                        } else if (window.metoffice.runConsentScript.consentPurposes[consentPurposeNbr]) {
                            continue outerLoop;
                        }
                    } else {
                        let consentPurposeNbr = parseInt(consentPurposes[j]);
                        if (consentBooleanOp === "or") {
                            if (window.metoffice.runConsentScript.consentPurposes[consentPurposeNbr]) {
                                window.metoffice.runConsentScript.executeScript(script);
                                break outerLoop;
                            }
                        } else if (!window.metoffice.runConsentScript.consentPurposes[consentPurposeNbr]) {
                            continue outerLoop;
                        }
                    }
                }
            }

            if (consentBooleanOp !== "or") {
                window.metoffice.runConsentScript.executeScript(script);
                break;
            }
        }
    },
    executeScript: function(script) {
        let newScript = document.createElement("script");
        newScript.type = "text/javascript";
        if (script.hasAttribute("data-src")) {
            console.log("loading external script "+script.dataset.src);
            newScript.src = script.dataset.src;
            newScript.addEventListener('load', window.metoffice.runConsentScript.findAndRunScript);
        } else {
            console.log("loading inline script");
            let textNode = document.createTextNode(script.innerText);
            newScript.appendChild(textNode);
            setTimeout(window.metoffice.runConsentScript.findAndRunScript, 0);
        }
        script.parentNode.insertBefore(newScript,script);
        script.remove();
    }
};

window.metoffice.runConsentScript.waitForConsent();
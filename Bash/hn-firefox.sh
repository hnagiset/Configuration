#! /usr/bin/env bash

PROFILE_DIR=$(mktemp -p /tmp -d firefox-profile.XXXXXX.d)

FIREFOX=firefox
CLEAN="rm -rf $PROFILE_DIR"
if flatpak list | grep Firefox; then
    FIREFOX="flatpak run --filesystem=$PROFILE_DIR org.mozilla.firefox"
fi

echo 'user_pref("browser.tabs.warnOnClose", true);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("dom.event.clipboardevents.enabled", false);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("dom.event.clipboardevents.enabled", false);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("dom.event.contextmenu.enabled", false);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("browser.urlbar.placeholderName", "DuckDuckGo!");' >> "$PROFILE_DIR/user.js"
echo 'user_pref("browser.newtabpage.enabled", false);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("datareporting.policy.firstRunURL", "");' >> "$PROFILE_DIR/user.js"
#echo 'user_pref("startup.homepage_welcome_url", "bing.com");' >> "$PROFILE_DIR/user.js"
echo 'user_pref("startup.homepage_welcome_url", "chatgpt.com");' >> "$PROFILE_DIR/user.js"
echo 'user_pref("browser.toolbars.bookmarks.visibility", "never");' >> "$PROFILE_DIR/user.js"
echo 'user_pref("mousewheel.with_alt.action", 5);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("general.autoScroll", true);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("ui.key.menuAccessKeyFocuses", false);' >> "$PROFILE_DIR/user.js"

# From Perplexity...
echo 'user_pref("toolkit.telemetry.enabled", false);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("toolkit.telemetry.unified", false);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("toolkit.telemetry.archive.enabled", false);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("toolkit.telemetry.updatePing.enabled", false);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("toolkit.telemetry.bhrPing.enabled", false);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("toolkit.telemetry.firstShutdownPing.enabled", false);' >> "$PROFILE_DIR/user.js"
echo '#// Disable health report' >> "$PROFILE_DIR/user.js"
echo 'user_pref("datareporting.healthreport.uploadEnabled", false);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("datareporting.healthreport.service.enabled", false);' >> "$PROFILE_DIR/user.js"
echo '#// Disable other data reporting' >> "$PROFILE_DIR/user.js"
echo 'user_pref("datareporting.policy.dataSubmissionEnabled", false);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("datareporting.sessions.current.clean", true);' >> "$PROFILE_DIR/user.js"

cp ~/.mozilla/firefox/*.default*/search.json.mozlz4 "$PROFILE_DIR"

$FIREFOX -profile "$PROFILE_DIR" -no-remote -new-instance \
    --new-tab "perplexity.ai" --new-tab "chatgpt.com"
#-private-window

$CLEAN

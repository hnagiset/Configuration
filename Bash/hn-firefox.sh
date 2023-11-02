#! /usr/bin/env bash

PROFILE_DIR=$(mktemp -p /tmp -d firefox-profile.XXXXXX.d)

echo 'user_pref("dom.event.clipboardevents.enabled", false);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("dom.event.contextmenu.enabled", false);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("browser.urlbar.placeholderName", "DuckDuckGo!");' >> "$PROFILE_DIR/user.js"
echo 'user_pref("browser.newtabpage.enabled", false);' >> "$PROFILE_DIR/user.js"
echo 'user_pref("datareporting.policy.firstRunURL", "");' >> "$PROFILE_DIR/user.js"
echo 'user_pref("startup.homepage_welcome_url", "bing.com");' >> "$PROFILE_DIR/user.js"

cp ~/.mozilla/firefox/*.default*/search.json.mozlz4 "$PROFILE_DIR"

firefox -profile "$PROFILE_DIR" -no-remote -new-instance #-private-window

rm -rf "$PROFILE_DIR"

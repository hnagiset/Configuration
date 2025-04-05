#! /usr/bin/env bash

DATA_DIR=$(mktemp -p /tmp -d chromium-data.XXXXXX.d)
CLEAN="rm -rf $DATA_DIR"

CHROMIUM=chromium-browser
if flatpak list | grep Chromium; then
    CHROMIUM="flatpak run --filesystem=$DATA_DIR org.chromium.Chromium"
elif command -v chromium-freeworld; then
    CHROMIUM=chromium-freeworld
fi

$CHROMIUM --user-data-dir="$DATA_DIR" --enable-features=UseOzonePlatform --ozone-platform=wayland --window-size=1200,1000 #--incognito

rm -rf "$DATA_DIR"

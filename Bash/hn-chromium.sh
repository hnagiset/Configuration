#! /usr/bin/env bash

DATA_DIR=$(mktemp -p /tmp -d chromium-data.XXXXXX.d)

if command -v chromium-freeworld; then
    EXE=chromium-freeworld
else
    EXE=chromium-browser
fi

$EXE --user-data-dir="$DATA_DIR" --enable-features=UseOzonePlatform --ozone-platform=wayland #--incognito

rm -rf "$DATA_DIR"

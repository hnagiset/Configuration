#! /usr/bin/env bash

PROFILE_DIR=$(mktemp -p /tmp -d firefox-profile.XXXXXX.d)

firefox -profile "$PROFILE_DIR" -no-remote -new-instance #-private-window

rm -rf "$PROFILE_DIR"

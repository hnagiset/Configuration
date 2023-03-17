#! /usr/bin/env bash

# hn-convert-epoch.sh
#  Convert the trailing number of all arguments from an epoch
#  timestamp to a human readable timestamp.

if [[ $# -lt 1 ]]; then
    printf "Usage: $(basename $0) args\n"
    exit 1
fi

# Create a list of timestamps
for i in "$@"; do
    # This millennium is 9 to 10 digits.
    stamp=$(echo "$i" | grep -oP '\d{9,10}')
    if [[ -n "$stamp" ]]; then
        STAMPS="$STAMPS $stamp"
    fi
done

# `xargs -n1' calls `echo' with one argument/timestamp
# `xargs' calls `echo' with all arguments
SORTED=$(printf "$STAMPS" | xargs -n1 | sort -u | xargs)

for i in $SORTED; do
    # Iterate through all arguments...
    for j in "$@"; do
        arg=$(echo "$j" | grep -P "$i")
        # Print argument if it contains timestamp
        if [[ -n "$arg" ]]; then
            printf "$arg: "
        fi
    done
    date -d @$i
done

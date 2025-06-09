## Usage: link_file SRC DST
link_file () {
    local source=$1
    local destination=$2

    if [[ ! -e "$source" ]]; then
        echo "$source is not present!"
        return 1
    fi

    if [[ -e "$destination" ]]; then
        if [ "$1" -ef "$2" ]; then
            echo "$destination is already present."
            return 0
        else
            local bkup="$destination.$(date +%s)"
            echo "Moving existing file to $bkup."
            mv "$destination" "$bkup"
        fi
    fi

    mkdir -p "$(dirname "$destination")"
    ln -s "$source" "$destination"
    echo "---"
    echo "Created $destination."
    echo "---"
}

## Usage: copy_file SRC DST
copy_file () {
    local source=$1
    local destination=$2

    if [[ ! -e "$source" ]]; then
        echo "$source is not present!"
        return 1
    fi

    if [[ -e "$destination" ]]; then
        echo "$destination is already present."
        return 0
    else
        mkdir -p "$(dirname "$destination")"
        cp -i "$source" "$destination"
        echo "---"
        echo "Created $destination."
        echo "---"
    fi
}

## Usage: clone_repo SRC DST
clone_repo () {
    local source=$1
    local destination=$2

    if [[ -e "$destination" ]]; then
        echo "$destination is already present."
        return 0
    else
        git clone "$source" "$destination"
        echo "---"
        echo "Created $destination."
        echo "---"
    fi
}

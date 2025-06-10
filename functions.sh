check_for_backups () {
    local tgt="$1"

    local base="$(basename "$tgt")"
    local dir="$(dirname "$tgt")"

    if [ -n "$(find "$dir" -maxdepth 1 -name "$base.*" -print -quit)" ]; then
        ls -1d "$tgt".1* 2> /dev/null
        ls -1d "$tgt".2* 2> /dev/null
    fi
}

## Usage: link_file SRC DST
link_file () {
    local src=$1
    local dst=$2

    if [[ ! -e "$src" ]]; then
        echo "$src is not present!"
        return 1
    fi

    if [[ -e "$dst" ]] || [[ -L "$dst" ]]; then
        if [ "$1" -ef "$2" ]; then
            echo "$dst is already present."
            check_for_backups "$dst"
            return 0
        else
            local bkup="backup.$(date +%s)"
            echo "Moving existing file to $bkup."
            mkdir "$bkup"
            mv "$dst" "$bkup"
            check_for_backups "$dst"
        fi
    fi

    mkdir -p "$(dirname "$dst")"
    ln -s "$src" "$dst"
    local retval=$?
    echo "---"
    echo "Created $dst."
    echo "---"
    return $retval
}

## Usage: copy_file SRC DST
copy_file () {
    local src=$1
    local dst=$2

    if [[ ! -e "$src" ]]; then
        echo "$src is not present!"
        return 1
    fi

    if [[ -e "$dst" ]]; then
        echo "$dst is already present."
        check_for_backups "$dst"
        return 0
    else
        mkdir -p "$(dirname "$dst")"
        cp -i "$src" "$dst"
        local retval=$?
        echo "---"
        echo "Created $dst."
        echo "---"
        return $retval
    fi
}

## Usage: clone_repo SRC DST
clone_repo () {
    local src=$1
    local dst=$2

    if [[ -e "$dst" ]]; then
        echo "$dst is already present."
        return 0
    else
        git clone "$src" "$dst"
        local retval=$?
        echo "---"
        echo "Created $dst."
        echo "---"
        return $retval
    fi
}

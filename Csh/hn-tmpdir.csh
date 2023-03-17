#! /bin/csh -f

if (`basename -- $0` =~ "hn-tmpdir.csh") then
    echo "$0 was run as an executable! Please source it instead."
    echo "tcsh> source $0"
else
    cd `mktemp -d`
    pwd
endif

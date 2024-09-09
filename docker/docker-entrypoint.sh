#!/bin/bash
set -e 

if [ "$(whoami)" == "root" ]; then
    chown -R h24:h24 /home/h24/workspace
    exec gosu mole "$@"
fi


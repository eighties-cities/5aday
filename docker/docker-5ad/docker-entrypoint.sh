#!/bin/bash
set -e 

if [ "$(whoami)" == "root" ]; then
    chown -R 5ad:5ad /home/h24/workspace
    exec gosu mole "$@"
fi


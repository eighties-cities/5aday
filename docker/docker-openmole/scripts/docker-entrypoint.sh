#!/bin/bash
set -e

if [ "$(whoami)" == "root" ]; then
    chown -R mole:mole /home/mole/workspace
    exec gosu mole "$@"
fi


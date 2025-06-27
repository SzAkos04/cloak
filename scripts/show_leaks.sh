#!/bin/bash

if [[ ! -f scripts/leak_summary.log ]]; then
    echo "scripts/leak_summary.log not found! Run run_valgrind.sh first."
    exit 1
fi

cat scripts/leak_summary.log

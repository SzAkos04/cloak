#!/bin/bash

LOGFILE="scripts/valgrind_$(date +%Y%m%d_%H%M%S).log"

echo "Running Valgrind..."
valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes --verbose --log-file="$LOGFILE" ./build/cloak ./test/test.ck

echo "Valgrind finished. Log saved to $LOGFILE"
echo "Extracting leak summary..."

grep -A 20 "LEAK SUMMARY:" "$LOGFILE" > scripts/leak_summary.log

echo "Leak summary saved to scripts/leak_summary.log"

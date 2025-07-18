#!/bin/bash

GREEN='\033[0;32m'
CYAN='\033[0;36m'
RESET='\033[0m'

mkdir -p logs/valgrind

LOGFILE="logs/valgrind/valgrind_$(date +%Y%m%d_%H%M%S).log"

echo -e "${CYAN}[INFO]${RESET} Running Valgrind..."
valgrind --leak-check=full     \
         --show-leak-kinds=all \
         --track-origins=yes   \
         --verbose             \
         --log-file="$LOGFILE" \
         ./build/cloak ./tests/test.ck

echo -e "${GREEN}[OK]${RESET} Valgrind finished. Log saved to $LOGFILE"
echo -e "${CYAN}[INFO]${RESET} Extracting leak summary..."

grep -A 20 "LEAK SUMMARY:" "$LOGFILE" > logs/valgrind/leak_summary.log

echo -e "${GREEN}[OK]${RESET} Leak summary saved to logs/valgrind/leak_summary.log"

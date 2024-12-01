#!/bin/bash
set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <day>"
    exit 1
fi

TOKEN=$(cat .token)
URL="https://adventofcode.com/2024/day/$1/input"
OUTPUT_FILE=$(printf "Inputs/%02d.txt" $1)

echo "Downloading input from $URL and saving it to $OUTPUT_FILE"

curl -s -b "session=$TOKEN" $URL -o $OUTPUT_FILE

echo "Done! 🎉"

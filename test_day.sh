#!/bin/bash

output_file=$(mktemp)
error_file=$(mktemp)

if [[ "$1" == "-a" ]]; then
    stack test --progress-bar=none
    exit 0
fi

eval "$(./resolve_date.sh "$@")"

echo "Year: $YEAR"
echo "Day: $DAY"

stack test --test-arguments="--match=/AoC/${YEAR}${DAY}/" --progress-bar=none

#!/bin/bash

output_file=$(mktemp)
error_file=$(mktemp)

if [[ "$1" == "-a" ]]; then
    stack run -- --all-days
    exit 0
fi

eval "$(./resolve_date.sh "$@")"

echo "Year: $YEAR"
echo "Day: $DAY"

stack run -- -d ${YEAR}${DAY}

#!/bin/bash

output_file=$(mktemp)
error_file=$(mktemp)

additional_flags=""
all_days_flag=false
for arg in "$@"; do
    if [[ "$arg" == "-a" || "$arg" == "--all-days" ]]; then
        all_days_flag=true
    elif [[ "$arg" != "$YEAR" && "$arg" != "$DAY" ]]; then
        additional_flags="$additional_flags $arg"
    fi
done

if [[ "$all_days_flag" == true ]]; then
    stack run -- --all-days $additional_flags
    exit 0
fi

eval "$(./scripts/resolve_date.sh "$@")"

echo "Year: $YEAR"
echo "Day: $DAY"



stack test --test-arguments="--match=/AoC/${YEAR}${DAY}/ $additional_flags" --progress-bar=none 

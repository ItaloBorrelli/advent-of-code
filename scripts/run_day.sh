#!/bin/bash

output_file=$(mktemp)
error_file=$(mktemp)

additional_flags=""
all_days_flag=false
eval "$(./scripts/resolve_date.sh "$@")"

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

echo "Year: $YEAR"
echo "Day: $DAY"

stack run -- -d ${YEAR}${DAY} $additional_flags

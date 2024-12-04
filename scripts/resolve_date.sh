#!/bin/bash

validate_date() {
    RESULT=0

    if [[ ! -z $year && ! $year =~ ^[0-9]{4}$ ]]; then
        echo "echo \"Invalid year: $year\""
        echo "echo \"Must be four digits long\""
        RESULT=1
    fi

    if [[ ! -z $day ]] && [[ ! $day =~ ^[0-9]{2}$ ]]; then
        echo "echo \"Invalid day: $day\""
        echo "echo \"Must be two digits long\""
        RESULT=1
    fi

    if [[ RESULT == 1 ]]; then
        echo "exit 1"
    fi

    return $RESULT
}

if [[ -f .env ]]; then
    source .env
fi

year=$YEAR
day=$DAY

validate_date "$year" "$day"

if [[ $? -ne 0 ]]; then
    exit 1
fi

if [[ -z $YEAR ]]; then
    YEAR=""
fi

if [[ -z $DAY ]]; then
    DAY=""
fi

# Loop through the command-line arguments
for arg in "$@"; do
    if [[ $arg =~ ^[0-9]{4}$ ]]; then
        YEAR=$arg
    elif [[ $arg =~ ^[0-9]{2}$ ]]; then
        DAY=$arg
    fi
done

if [[ -z $YEAR ]]; then
    YEAR=${YEAR:-$(date +%Y)}
fi

if [[ -z $DAY ]]; then
    DAY=${DAY:-$(date +%d)}
fi

# Output YEAR and DAY to be evaluated by the calling script
echo "YEAR=$YEAR"
echo "DAY=$DAY"

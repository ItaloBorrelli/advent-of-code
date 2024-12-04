#!/bin/bash

eval "$(./resolve_date.sh "$@")"

echo "Year: $YEAR"
echo "Day: $DAY"

echo "This will initialize the folder and file structure for the given day."
echo "Are you satisfied with the above values?"

read -p "Note: Content in existing files will not be deleted. (y/n) " confirm
if [[ $confirm != [yY] ]]; then
    echo "Change the .env file to provide export the desired YEAR and DAY OR provide both/either value in the command line input."
    echo "Otherwise todays day and year will be used."
    exit 1
fi

echo

INPUT_DIR="inputs/$YEAR/$DAY"

mkdir -p $INPUT_DIR

INPUT_FILE="$INPUT_DIR/input.txt"
if [ ! -f $INPUT_FILE ]; then
    touch $INPUT_DIR/input.txt
    echo "'input.txt' created in '$INPUT_DIR'"
fi

TEST_ANSWER_FILE="$INPUT_DIR/test-answers.txt"
if [ ! -f $TEST_ANSWER_FILE ]; then
    touch $TEST_ANSWER_FILE
    echo "Couldn't run Part A!" >> $TEST_ANSWER_FILE
    echo "Couldn't run Part B!" >> $TEST_ANSWER_FILE
    echo "'test-answers.txt' created in '$INPUT_DIR'"
fi

TEST_INPUT_FILE="$INPUT_DIR/test-input.txt"
if [ ! -f $TEST_INPUT_FILE ]; then
    touch $TEST_INPUT_FILE
    echo "'test-input.txt' created in '$INPUT_DIR'"
fi

echo

SOLVE_DIR="src/AOC/Y$YEAR"
SOLVE_FILE_NAME="Day$DAY.hs"
SOLUTION_FILE="$SOLVE_DIR/$SOLVE_FILE_NAME"
if [ ! -f $SOLUTION_FILE ]; then
    mkdir -p $SOLVE_DIR
    cp ./Day.hs.template $SOLUTION_FILE
    sed -i "s/YYYYY\.DayXX/Y$YEAR.Day$DAY/g" $SOLUTION_FILE
    echo "Created $SOLUTION_FILE"
else
    echo "Solution file already exists."
fi

echo

MAIN_FILE="app/Main.hs"
IMPORT_STATEMENT="import qualified AOC.Y$YEAR.Day$DAY as Y${YEAR}Day${DAY}"
IMPORT_PREFIX=${IMPORT_STATEMENT:0:32}
if ! grep -q "$IMPORT_PREFIX" "$MAIN_FILE"; then
    sed -i "/--- Day imports/ { :a; n; /^import /! { s/.*/&$IMPORT_STATEMENT\n/; b }; ba }" "$MAIN_FILE"
    echo "Added import statement to $MAIN_FILE"
else
    echo "Matching import statement already exists in $MAIN_FILE"
fi

INSERT_STATEMENT="(${YEAR}${DAY}, (Y${YEAR}Day${DAY}.runDay, \"inputs/${YEAR}/${DAY}/input.txt\"))"
if ! grep -q "$INSERT_STATEMENT" "$MAIN_FILE"; then
    sed -i -E "s/(input.txt\"\)\))$/\1,/g" "$MAIN_FILE"
    sed -i "/-- Insert new days here/,/]/ { 
        /^[[:space:]]*]/ i \      $INSERT_STATEMENT
    }" "$MAIN_FILE"
    echo "Added runDay to test list in $MAIN_FILE"
else
    echo "Day entry already exists in the list in $MAIN_FILE"
fi

SPEC_FILE="test/Spec.hs"
TEST_STATEMENT="it \"${YEAR}${DAY}\" $ do runDay (\"${YEAR}\", \"${DAY}\")"
if ! grep -q "$TEST_STATEMENT" "$SPEC_FILE"; then
    sed -i "/-- Add new tests here/ a \    $TEST_STATEMENT" "$SPEC_FILE"
    echo "Added test entry to $SPEC_FILE"
else
    echo "Test entry already exists in the list in $SPEC_FILE"
fi

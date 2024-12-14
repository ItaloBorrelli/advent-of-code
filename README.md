# Advent of Code

This repository is intended as a framework for writing and running Advent of Code challenge code in Haskell. I started with an empty repo then realized [samcoy3's template](https://github.com/samcoy3/advent-of-code-template) had a lot of what I wanted to do, so I created this as a fork, though it also adds some utilities to automatically generate files for test code.

## Setup

Clone this repository.

To run the `aoc` script easier, add the scripts folder to your path by adding the following to your shell rc file, substituting `$HOME/advent-of-code` with the path to your local repository if necessary (e.g.`.zshrc`, `.bashrc`):

```txt
export PATH="$HOME/advent-of-code/scripts:$PATH"
```

It is also suggested to set at least `export YEAR=2024` in a `.env` file, like in the `.env.example`. You can also add a day as in `export DAY=01`.

Additionally, because this was setup with VS Code in mind you can also set `export PROMPT_VSCODE=true` in the `.env`. This will then prompt to run `code` with the input files and solution code file in VS Code immediately.

### GHCup & Stack

This was created with `GHCup` and `stack` was installed through `GHCup`. Using `cabal` for this project, or not using `GHCup` is left as an exercise for the reader. To install follow [the official instructions](https://www.haskell.org/ghcup/install/).

Then run `stack build`.

### VS Code

Install the following to use `fourmolu` and `hlint`. Installing `apply-refact` can be used to also refactor code linted with `hlint`:

```bash
stack install fourmolu hlint apply-refact
```

Then open VS Code and install recommended or desired extensions.

## `aoc` Script

You can use the `aoc` script to initialize days, run tests with the test input and run the days code. It should be run from the base of this repository. Run with `-h` to get the help.

The day and year is resolved with the following precedence from highest:

1. **Value set with flag parameters:** Use `-d ##` for the day and `-y ####` for the year.
2. **Value set in `.env`:** The executable will try to source the `.env` file, which should `export` the `YEAR` and `DAY` variables if set.
3. **Current day and year in EST (-5):** With no provded day and year, the current day and year will be used, which is  useful when doing the challenges on a daily basis in December.

Note that the day can be set with one method and year with the other.

### Initialize day

Run `aoc -g` or `aoc --init` to generate the files for the day.

This will:

* Create `input.txt`, `test-answers.txt` and `test-input.txt` files in `inputs/$YEAR/$DAY`,
* Copy a template day (`Day.hs.template`) as `src/AOC/Y${YEAR}/Day${Day}.hs`,
* Add an import statement to `app/Main.hs`,
* Add `Y${YEAR}Day${DAY}.runDay` and the default input file in `app/Main.hs`, and
* Add a test entry to`test/Spec.hs`.

After that, run `stack build`. You can then put your inputs in the files and start working on the problem.

NOTE: The input files are not tracked, as requested by Eric Wastl on the [Advent of Code about page](https://adventofcode.com/about) (see "Can I copy/redistribute part of Advent of Code?").

### Test day

Put the example input given into `test-input.txt` and put the example output for part A and part B into the first and second line respectively of `test-answers.txt`.

Then run `aoc -t` or `aoc --test`. The days code will be run with the input of `test-input.txt` and compared to the values in `test-answers.txt`.

This runs `stack test --test-arguments="--match=/AoC/${YEAR}${DAY}/"`.

Adding the `-a` or `--all-days` flag runs specs on every day.

### Run day

Put your puzzle input into `input.txt` and run the day with `aoc -r` or `aoc --run`. You can also provide a different input file with the `-i ${INPUT_FILE}` or `--input ${INPUT_FILE}`.

This runs `stack run -- -d ${YEAR}${DAY} --timings` or `stack run -- -d ${YEAR}${DAY} -i ${INPUT_FILE} --timings` depending on whether an input file was provided.

Adding the `-a` or `--all-days` flag runs every day.

## Other

* `hie.yaml` was created with `gen-hie > hie.yaml`. First install `stack install implicit-hie`.
* The `cabal` file is provided if you want to use `cabal` instead of `stack`.

# Advent of Code

This repository is intended as a framework for writing and running Advent of Code challenge code in Haskell. I started with an empty repo then realized [samcoy3's template](https://github.com/samcoy3/advent-of-code-template) had a lot of what I wanted to do, so I created this as a fork, though it also adds some utilities to automatically generate files for test code.

## Setup

Clone this repository.

### GHCup & Stack

This was created with `GHCup` and `stack` was installed through `GHCup`. Using `cabal` for this project, or not using `GHCup` is left as an exercise for the reader. To install follow [the official instructions](https://www.haskell.org/ghcup/install/).

Then run `stack build`.

### VS Code

Install the following if you will be using the `haskell-linter` (`hoovercj.haskell-linter`) and `ormolu` (configured for VS Code to be used through the `haskell.haskell` extension). `apply-refact` can be used so  :

```bash
stack install hlint ormolu apply-refact
```

Then open VS Code and install recommended or desired extensions.

## Initialize day

To get a day started, run `./scripts/aoc --init -y YYYY -d DD`. Or `cp .env.example .env` and set the `YEAR` and/or `DATE` value in there. It could be helpful to only set the year and run with the day parameter. In other words, set `export YEAR=20YY` in `.env` then run `./scripts/init_day.sh DD`. With no day or year set, the current day and year will be used, which is useful when doing the challenges on a daily basis in December.

This creates `input.txt`, `test-answers.txt` and `test-input.txt` files in `inputs/YYYY/DD`, copies a template day as `src/AOC/Y${YYYY}/Day${DD}.hs` and adds lines to `Main.hs` and `Spec.hs` so that it is runnable through `stack run -- -d YYYYDD` and `stack test --test-arguments="--match=/AoC/YYYYDD/"`, which are also provided as scripts.

After that, run `stack build`. You can then put your inputs in the files and start working on the problem.

NOTE: The input files are not tracked, as requested by Eric Wastl on the [Advent of Code about page](https://adventofcode.com/about) (see "Can I copy/redistribute part of Advent of Code?").

## Test day

Put the example input given into `test-input.txt` and put the example output for part A and part B into the first and second line respectively of `test-answers.txt`. Then run `./scripts/aoc -t` or `./scripts/aoc --test`. As with `./scripts/aoc --init` script, it can be run with or without the `-y YYYY` and `-d DD` parameters, and you can use exports in `.env`.

## Run day

Once your tests pass, put your puzzle input into `input.txt` and run the day with `./scripts/aoc -r` or `./scripts/aoc --run`. As with `./scripts/aoc --init` and `./scripts/aoc --test` scripts, it can be run with or without the `-y YYYY` and `-d DD` parameters, and you can use exports in `.env`.

## Other

`hie.yaml` was created with `gen-hie > hie.yaml`. First install `stack install implicit-hie`.

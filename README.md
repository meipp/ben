# ben - run benchmarks

`ben` is an easy to use benchmarking tool that takes a set of programs and a set of input files and runs every program on every input file measuring their running time.

The programs benchmarked can be any valid shell command. On top of that, `ben` allows for classifying and grouping program runs by their produced output, parallelization, timeouts, JSON exports and repeated re-runs of benchmark for empirical robustness.

## Usage
```
ben (-p|--program CMD) (-s|--source PATH) [-c|--classifier CMD]
    [-j|--jobs N] [-t|--timeout SECONDS] [-J|--json] [-r|--repetitions N]
```

## Example 1 - Fibonacci
Suppose we have a fast fibonacci implementation ([good.py](./examples/fibonacci/good.py)) and a slow one ([bad.py](./examples/fibonacci/bad.py)). We want to benchmark their performance on the inputs `10` (easy), `40` (bad.py will be slow here) and `100` (bad.py is going to timeout). This example needs `python3` installed.

We benchmark the programs `./good.py` and `./bad.py` on the input files in [examples/fibonacci/inputs](./examples/fibonacci/inputs) with a timeout of `60` seconds for each program call.
```bash
ben -p ./good.py -p ./bad.py -s ./inputs/ -t 60
```
The output will look something like this:
```
| Program   | Timeouts | Errors | Time (ms) |
|-----------|----------|--------|-----------|
| ./good.py |        0 |      0 |        93 |
| ./bad.py  |        1 |      0 |     90199 |
```

## Example 2 - String Solving
Let's benchmark the two string solvers CVC4 and Z3 on the files in [examples/string-solving/test-instances](./examples/string-solving/test-instances) and count how often they output `sat` and `unsat`. This example needs `cvc4`, `z3` and `grep` installed.

```bash
ben \
    -s ./test-instances/ \
    -p cvc4 -p z3 \
    -c "Sat: grep ^sat$" -c "Unsat: grep ^unsat$" \
    -t 20
```
The output will look something like this:
```
| Program | Sat | Unsat | Timeouts | Errors | Time (ms) |
|---------|-----|-------|----------|--------|-----------|
| cvc4    |   4 |     3 |        1 |      0 |     20106 |
| z3      |   4 |     4 |        0 |      0 |       195 |
```

## Installation
To install locally run
```bash
git clone https://github.com/meipp/ben
cd ben
stack install
```
This will install `ben` to `$HOME/.local/bin/ben`. Make sure to have `$HOME/.local/bin/` in your `$PATH`.

### Running without installing
To run without installing, you can build locally:
```bash
git clone https://github.com/meipp/ben
cd ben
stack build
```
Now instead of `ben -p ...`, run `stack run -- -p ...`.

## Command line options

### `-p`, `--program`
Adds a program to include in the benchmark. Multiple uses allowed. At least one use mandatory.

```bash
ben -p program1 -p program2 -s ...
```
will run a benchmark on these two programs.

`-p` takes a command that is evaluated in the system shell. This means commands can be complex:
```bash
ben -p "sleep 2 && program1 arg1 arg2" ...
```

### `-s`, `--source`
Adds a path to the list of input files. Multiple uses allowed. At least one use mandatory.

### `-c`, `--classifier`
Adds a program/command to the list of classifiers. Multiple uses allowed.

A classifier is a program/command to evaluate against a program's stdout. The classifier returns true or false return codes.
Just like `-p`, classifiers can be complex shell commands.

```bash
ben -c "grep ^sat$" ...
```
will run a benchmark with a classifier that returns true for every program run where the output contains a line `"sat"`.

### `-j`, `--jobs`
Number of jobs to run in parallel, i.e. the number of program invocations to do at a time. The default is 1.

The value should not exceed the machine's number of available cores/threads. Otherwise all invocations can be subject to process switching/scheduling behavior and wall-clock time measurements are affected.

### `-t`, `--timeout`
Sets a timeout (in seconds) for every program call to be run. After exceeding the timeout, the program is terminated and its run is counted as a *Timeout*. The default is 60 seconds. The timeout cannot be disabled. `-t` allows decimals.

```bash
ben -t 0.5 ...
```
runs a benchmark with a timeout of half a second.

### `-J`, `--json`
Produces a JSON dump of all measurements and prints it in the last line of output. The JSON is not prettified.

The JSON output for [Example 1](#example-1---fibonacci)
```bash
ben -p ./good.py -p ./bad.py -s ./inputs/ -t 60 -J
```
will look something like
```json
[{"classifications":[],"command":"./good.py ./inputs/input-2.txt","program":"./good.py","status":"0","stderr":"","stdout":"102334155\n","time":15},{"classifications":[],"command":"./good.py ./inputs/input-1.txt","program":"./good.py","status":"0","stderr":"","stdout":"55\n","time":13},{"classifications":[],"command":"./good.py ./inputs/input-3.txt","program":"./good.py","status":"0","stderr":"","stdout":"354224848179261915075\n","time":14},{"classifications":[],"command":"./bad.py ./inputs/input-2.txt","program":"./bad.py","status":"0","stderr":"","stdout":"102334155\n","time":17718},{"classifications":[],"command":"./bad.py ./inputs/input-1.txt","program":"./bad.py","status":"0","stderr":"","stdout":"55\n","time":14},{"classifications":[],"command":"./bad.py ./inputs/input-3.txt","program":"./bad.py","status":"timeout","stderr":"","stdout":"","time":60021}]
```

### `-r`, `--repetitions`
Number of times to re-run each program invocation. A higher number of repetitions usually leads to more stable and reproducible results. The default is 1.

### `-h`, `--help`
Displays help.

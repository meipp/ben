# ben - run benchmarks

## Example 1 - Fibonacci
Suppose we have a fast fibonacci implementation ([good.py](./examples/fibonacci/good.py)) and a slow one ([bad.py](./examples/fibonacci/bad.py)). We want to benchmark their performance on the inputs `10` (easy), `40` (bad.py will be slow here) and `100` (bad.py is going to timeout). This example needs `python3` installed.

We benchmark the programs `./good.py` and `./bad.py` on the input files in `./inputs/` with a timeout of `60` seconds for each program call.
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

# ben - run benchmarks

## Example - String Solving
Let's benchmark the two string solvers CVC4 and Z3 on the files in [examples/string-solving/test-instances](./examples/string-solving/test-instances) and count how often they output `sat` and `unsat`.

```bash
cd examples/string-solving
ben \
    -s ./test-instances \
    -p cvc4 -p z3 \
    -c "sat: grep ^sat$" -c "unsat: grep ^unsat$"
```
The produced output will look something like this:
```
| Program | Sat | Unsat | Timeouts | Errors | Time (ms) |
|---------|-----|-------|----------|--------|-----------|
| cvc4    |   4 |     3 |        1 |      0 |      2089 |
| z3      |   4 |     4 |        0 |      0 |       187 |
```

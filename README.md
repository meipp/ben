# ben - run benchmarks

## Example - String Solving
Let's benchmark the two string solvers CVC4 and Z3 on the files in [examples/string-solving/test-instances](./examples/string-solving/test-instances) and count how often they output `sat` and `unsat`.

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

ben \
    -s ./test-instances \
    -p cvc4 -p z3 \
    -c "sat: grep ^sat$" -c "unsat: grep ^unsat$"

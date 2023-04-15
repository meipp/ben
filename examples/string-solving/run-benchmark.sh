ben \
    -s ./test-instances \
    -p cvc4 -p z3 \
    -c "Sat: grep ^sat$" -c "Unsat: grep ^unsat$"

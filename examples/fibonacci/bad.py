#!/usr/bin/python3

# Bad (exponential time) implementation of fibonacci
# Reads a number n from the file passed in argv[1] and calculates the nth fibonacci number

from sys import argv

def fib(n):
    if n <= 1:
        return n
    else:
        return fib(n-1) + fib(n-2)

if __name__ == "__main__":
    with open(argv[1], "r") as f:
        n = int(f.read())
    print(fib(n))

#!/usr/bin/python3

# Good (linear time) implementation of fibonacci
# Reads a number n from the file passed in argv[1] and calculates the nth fibonacci number

from sys import argv

def fib(n):
    (a, b) = (0, 1)
    for i in range(n):
        (a, b) = (b, a + b)
    
    return a

if __name__ == "__main__":
    with open(argv[1], "r") as f:
        n = int(f.read())
    print(fib(n))

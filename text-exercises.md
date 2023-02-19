# Milestone 3 Short-answer questions

## Exercise 5

1. Benefits of `compiler-m3` vs. `compiler-m2`
    - Benefits of M3
      - Allows the use of registers for a slightly faster execution time
      - Smaller compiled x64 program
      - Requires potentially less memory to run
    - Benefits of M2
      - Has faster compile time because it doesn't need to do conflict analysis and register allocation on those conflicts
      - Simpler compilation and compiler

2. `compile-m3` is optimal compared to `compile-m2` since the usage of registers is required if we want to run programs efficiently.

3. From `compare-compile.rkt`, we can see that the execution time is slightly faster for `compile-m3`. The average execution time over 3 runs (with format `real(cpu)` time):
    - Program 1: `222(6)`ms for M3 vs. `364(9)`ms for M2
    - Program 2: `246(7)`ms for M3 vs. `291(7)`ms for M2

    Surprisingly, the execution time is not much faster. But the compile time is much slower (with program 3 reaching `15000`ms vs. `90`ms for M2).
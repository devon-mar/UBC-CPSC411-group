# Milestone 3 Short-answer questions

## Exercise 5

1. Benefits of `compiler-m3` vs. `compiler-m2`
    - Benefits of M3
      - Allows the use of registers for faster execution time
      - Allows sharing registers for non-conflicting variables
      - Smaller compiled x64 program
      - Requires potentially less memory to run
    - Benefits of M2
      - Has faster compile time because it doesn't need to do conflict analysis and register allocation on those conflicts
      - Simpler compilation and compiler

2. `compile-m3` makes more optimized code compared to `compile-m2` from the usage of registers. However, the register allocation algorithm for m3 is still not optimal. For one, it assigns alocs to registers or fvars without taking account the frequency of their usage. For example, we may be assigning an aloc that is used once to a register, while assigning an aloc that is used frequently to an fvar. Another point is that we may get redundant statements from the assignments such as `(set! r9 r9)` which we do not remove. Lastly, when we add branches and loops to the language, there will be cases that two non-conflicting alocs may be determined to be conflicting due to Rice's Theorem.

3. From `compare-compile.rkt`, we can see that the execution time is slightly faster for `compile-m3`. The average execution time over 3 runs (with format `real(cpu)` time):
    - Program 1: `222(6)`ms for M3 vs. `364(9)`ms for M2
    - Program 2: `246(7)`ms for M3 vs. `291(7)`ms for M2

    Surprisingly, the execution time is not much faster. But the compile time is much slower (with program 2 reaching `15000`ms vs. `90`ms for M2).
# Milestone 2 Short-answer questions

## Exercise 14

`sequentialize-let` was simplified by the ability to nest `tail`s.

If a `tail` had a sequence of instructions, such as this modified `values-unique-lang-v2` grammar shown below: 

```
tail ::= value
      |  (let ([aloc value] ...) tail ...)
```

we would have to worry about restoring the old value of an `aloc` once it goes out of scope:

```racket
(module
  (let ([x 1])
    (let ([x 2]) (+ x 2))
    ;; x should be 1 here!
    x))
```

Since a tail in `values-unique-lang-v3` is either a `value`, or a `let`, which must contain a tail, we don't have
to worry about values in previous scopes since they are effectively gone once we enter a new scope:

```racket
(module
  (let ([x 1])
    ;; There's no way to access the above value of `x`
    ;; after it has been shadowed by the below declaration since
    ;; a let must only contain a `tail`.
    (let ([x 2])
      x)))
```

Therefore, when converting a `let` to a sequence of `set!` instructions, we can simply overwrite the previous value.


`select-instructions` was also simplified by the ability to nest tails. Since an `imp-cmf-lang-v3` instruction may map to multiple
`asm-lang-v2` instructions, the ability to nest `tail`s, notably `begins`, allowed every function in `select-instructions` to return a single
instruction, instead of returning multiple values then using `append`, etc.


# Milestone 3 Short-answer questions

1. Benefits of `compiler-m3` vs. `compiler-m2`
    - Benefits of M3
      - Allows the use of registers for theoretically faster execution time
      - Allows sharing registers for non-conflicting variables
      - Smaller compiled x64 program
      - Requires potentially less memory to run
    - Benefits of M2
      - Has faster compile time because it doesn't need to do conflict analysis and register allocation on those conflicts
      - Simpler compilation and compiler

2. `compile-m3` makes more optimized code compared to `compile-m2` from the usage of registers. However, the register allocation algorithm for m3 is still not optimal. For one, it assigns alocs to registers or fvars without taking account the frequency of their usage. For example, we may be assigning an aloc that is used once to a register, while assigning an aloc that is used frequently to an fvar. Another point is that we may get redundant statements from the assignments such as `(set! r9 r9)` which we do not remove. Also another point is how we don't take into account the values of the registers, where registers that share values such as `(set! r12 9) (set! r13 9)` can share locations.  Lastly, when we add branches and loops to the language, there will be cases that two non-conflicting alocs may be determined to be conflicting due to Rice's Theorem.

3. From `compare-compile.rkt`, we can see that the execution time is slightly faster for `compile-m3`. The average execution time over 3 runs (with format `real(cpu)` time):
    - Program 1: `222(6)`ms for M3 vs. `364(9)`ms for M2
    - Program 2: `246(7)`ms for M3 vs. `291(7)`ms for M2

    Surprisingly, the execution time is not much faster. But the compile time is much slower (with program 2 reaching `15000`ms vs. `90`ms for M2). The lack of major difference in execution time is likely because the instructions don't directly use memory but use the CPU's cache which makes for a minor difference for small programs such as the one tested.
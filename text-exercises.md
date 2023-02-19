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


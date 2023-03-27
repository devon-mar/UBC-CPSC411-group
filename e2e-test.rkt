#lang racket

(module+ test
  (require
    rackunit
    "compiler.rkt"
    cpsc411/compiler-lib
    cpsc411/2c-run-time)

  (current-pass-list
    (list
      uniquify
      sequentialize-let
      normalize-bind
      impose-calling-conventions
      select-instructions
      uncover-locals
      undead-analysis
      conflict-analysis
      assign-call-undead-variables
      allocate-frames
      assign-registers
      assign-frame-variables
      replace-locations
      optimize-predicates
      implement-fvars
      expose-basic-blocks
      resolve-predicates
      flatten-program
      patch-instructions
      generate-x64
      wrap-x64-boilerplate
      wrap-x64-run-time))

  ;; Check that program compiles and executes properly to value
  (define-check (check-execute p v)
    (check-equal? (execute p) v)
    (check-equal?
      (parameterize ([current-parameter-registers '()] [current-assignable-registers '()])
        (execute p))
      v))

  (check-execute
    '(module
      (define fact
        (lambda (n acc)
          (if (<= n 1)
              acc
              (let ([n (- n 1)]
                    [acc (* acc n)])
                (call fact n acc)))))
      (let ([x (call fact 8 1)]
            [y (call fact 5 1)]) (+ x y)))
    40440)
  )

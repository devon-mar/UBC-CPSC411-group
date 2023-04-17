#lang racket

(module+ test
  (require
    rackunit
    "compiler.rkt"
    cpsc411/compiler-lib
    cpsc411/ptr-run-time)

  (current-pass-list
    (list
      uniquify
      implement-safe-primops
      implement-safe-call
      define->letrec
      optimize-direct-calls
      dox-lambdas
      uncover-free
      convert-closures
      optimize-known-calls
      hoist-lambdas
      implement-closures
      specify-representation
      remove-complex-opera*
      sequentialize-let
      normalize-bind
      impose-calling-conventions
      select-instructions
      expose-allocation-pointer
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
      implement-mops
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

  ;; base cases
  (check-execute '(module 42) 42)
  (check-execute '(module #t) #t)
  (check-execute '(module #f) #f)
  (check-execute '(module empty) 'empty)
  (check-execute '(module (void)) '(void))
  (check-execute '(module (error 15)) '(error 15))
  (check-execute '(module #\J) #\J)
  (check-execute '(module (cons 4 #t)) (cons 4 #t))
  (check-execute '(module (call cons 4 #t)) (cons 4 #t))
  (check-execute '(module (call make-vector 4)) (make-vector 4))

  (check-execute
    '(module
      (define fact
        (lambda (n acc)
          (if (call <= n 1)
              acc
              (let ([n (call - n 1)]
                    [acc (call * acc n)])
                (call fact n acc)))))
      (let ([x (call fact 8 1)]
            [y (call fact 5 1)]) (call + x y)))
    40440)
  )

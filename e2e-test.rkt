#lang racket

;; E2E tests

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

  ;; Check that program compiles and executes to error with code
  (define-check (check-error p code)
    (check-equal?
      (car (nasm-run/error-string+code (compile p)))
      code))

  ;; base cases
  (check-execute '(module 42) 42)
  (check-execute '(module #t) #t)
  (check-execute '(module #f) #f)
  (check-execute '(module empty) '())
  (check-execute '(module (void)) eof)
  (check-error '(module (error 15)) 15)
  (check-execute '(module #\J) #\J)
  (check-execute '(module (call cons empty (call cons 2 #\B))) (cons '() (cons 2 #\B)))
  (check-execute '(module (call cons 4 #t)) (cons 4 #t))
  (check-execute '(module (call make-vector 4)) (make-vector 4))

  ;; vector operations
  (check-execute
    '(module
      (let ([vec (call make-vector 4)])
        (let ([a (call vector-set! vec 2 178)]
              [b (call vector-set! vec 3 -44)])
          (call - (call vector-ref vec 2) (call vector-ref vec 3)))))
    222)

  ;; vector lambda
  (check-execute
    '(module
      (let ([vec (call make-vector 10)])
        (let ([fn (lambda (x)
                    (let ([a (call vector-set! vec 5 (call + (call vector-ref vec 5) x))])
                      (call vector-ref vec 5)))])
          (let ([fn2 (lambda ()
                      (let ([a (call vector-set! vec 5 (call * (call vector-ref vec 5) -7))])
                        fn))])
            (call
              (call
                (lambda ()
                  (let ([a (call vector-set! vec 5 (call + (call vector-ref vec 5) 66))])
                    (call
                      (call
                        (lambda (z)
                          (let ([a (call vector-set! vec 5 (call - (call vector-ref vec 5) z))])
                            fn2))
                        17)))))
              99)))))
    -244)

  ;; cons operations
  (check-execute
    '(module
      (let ([cons (call cons 5 #t)])
        (if (call cdr cons)
            (call - (call car cons) -8)
            (error 2))))
    13)

  ;; lambda in cons
  (check-execute
    '(module
      (let ([a 3])
        (let ([cons (call cons 5 (lambda (x) (call + x (call - x a))))])
          (call (call cdr cons) (call car cons)))))
    7)

  ;; lambda in vector
  (check-execute
    '(module
      (let ([vec (call make-vector 20)]
            [a 10])
        (let ([a (call vector-set! vec 15 (lambda (x) (call * x (call - x a))))])
          (call (call vector-ref vec 15) 6))))
    -24)

  ;; lambda - overide
  (check-execute
    '(module
      (let ([+ (lambda (x y) (call + x (call + x y)))]
            [lambda (lambda (x y) (call * x y))]
            [let (lambda (x y) (call + x y))])
        (call + (call lambda 2 3) (call let -9 8))))
    11)

  ;; lambdas
  (check-execute
    '(module
      (define func1
        (lambda (fn x y)
          (call (call fn x y) (call * x y))))
      (let ([func2 (lambda (x y)
                     (lambda (a) (call - a (call + x (call * y y)))))])
        (call func1 func2 17 7)))
    53)

  ;; call with lambda
  (check-execute
    '(module
      (call
        (lambda (fn1 fn2 fn3 x y z)
          (call -
            (call fn1 fn2 x)
            (call fn3 y y (lambda () z))))
        (lambda (fn x)
          (call (lambda (x y) (call - x y)) 10 (call fn x)))
        (lambda (x)
          (call + x x))
        (lambda (x y fn)
          (call * (call - x y) (call fn)))
        10
        17
        51))
    -10)

  ;; lambda - bad arity
  (check-error
    '(module
      (let ([func (lambda (x y)
                    (call + x (call + x y)))])
        (call func 2)))
    125)

  ;; lambda - bad arity indirect
  (check-error
    '(module
      (let ([func (lambda (a)
                    (lambda (x y)
                      (call + x (call + a y))))])
        (call (call func 7) 9)))
    125)

  ;; lambda - free variables
  (check-execute
    '(module
      (let ([t 10]
            [fn (lambda (x y z) (call * (call - x z) y))])
          (let ([fn (let ([a 20]
                          [b 7])
                 (lambda (x y) (call * (call + (call fn x t a) a) (call - y b))))])
        (call fn -12 9))))
    16)


  ;; factorial
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
            [y (call fact 5 1)])
        (call + x y)))
    40440)
  )

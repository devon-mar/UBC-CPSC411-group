#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time

  "uniquify.rkt"
  "sequentialize-let.rkt"
  "normalize-bind.rkt"
  "select-instructions.rkt"
  "assign-homes.rkt"
  "flatten-begins.rkt"
  "patch-instructions.rkt"
  "implement-fvars.rkt"
  "generate-x64.rkt")

(provide compile-m2)

;; M3 Exercise 5
;; Values-lang-v3 -> x64
;; Compile a Values-lang program into x64 program with M2 code
(define (compile-m2 p)
  (parameterize ([current-pass-list
                  (list uniquify
                        sequentialize-let
                        normalize-bind
                        select-instructions
                        assign-homes
                        flatten-begins
                        patch-instructions
                        implement-fvars
                        generate-x64
                        wrap-x64-run-time
                        wrap-x64-boilerplate)])
                (compile p)))

(module+ test
  (require rackunit)

  (check-equal? (nasm-run/read (compile-m2 '(module 5))) 5)
  (check-equal? (nasm-run/read (compile-m2 '(module (let ([x 10]) (+ x 9))))) 19)

  (define p1
    (compile-m2
      '(module
        (let ([x (let ([y 3]) (* y 2))]
              [a (let ([y 3] [x 10]) (+ x y))]
              [b 4])
          (let ([z (+ x 9)]
                [a (* a b)]
                [c (+ b 7)])
            (let ([z (* z a)]
                  [t (let ([i (let () 14)]) i)])
              (* x z)))))))

  (check-equal? (nasm-run/read p1) 4680)
  )
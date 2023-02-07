#lang racket

(module+ test
  (require rackunit cpsc411/compiler-lib cpsc411/langs/v2 "compiler.rkt")

  ; interp-paren-x64 tests
  (define (check-interp-paren-x64 p v)
    (check-equal? (interp-paren-x64 p) v))
  
  (check-interp-paren-x64 '(begin (set! rax 1)) 1)
  (check-not-exn (thunk (interp-paren-x64 '(begin (set! rax rbp)))))
  (check-interp-paren-x64 '(begin (set! (rbp - 8) 22) (set! rax (rbp - 8))) 22)
  (check-interp-paren-x64
    '(begin (set! rax 9) (set! (rbp - 8) 14) (set! rax (* rax (rbp - 8)))) 126)
  (check-interp-paren-x64
    `(begin
      (set! rax 1)
      (set! rbx 7)
      (set! rcx 9)
      (set! (rbp - 0) 4)
      (set! (rbp - 16) rbx)
      (set! rdx (rbp - 16))
      (set! rax (+ rax 4))
      (set! rdx (* rdx rcx))
      (set! rdx (+ rdx (rbp - 16)))
      (set! rdx (* rdx (rbp - 0)))
      (set! rax (+ rax rdx)))
    29))
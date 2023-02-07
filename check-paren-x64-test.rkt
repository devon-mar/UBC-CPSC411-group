#lang racket

(module+ test
  (require rackunit cpsc411/compiler-lib cpsc411/langs/v2 "compiler-m2.rkt")

  ; check-paren-x64 tests
  (define (check-paren-x64-success p)
    (check-equal? (check-paren-x64 p) p))
  (define (check-paren-x64-fail p)
    (check-exn exn:fail? (thunk (check-paren-x64 p))))
  
  (check-paren-x64-fail '())
  (check-paren-x64-fail '(begin))
  (check-paren-x64-fail `(begin (set! rax "hi")))
  (check-paren-x64-fail `(begin (set rax 0)))
  (check-paren-x64-fail '(begin (set! rbx 0)))
  (check-paren-x64-fail `(begin (set! (rbp - 0) ,(+ (max-int 32) 1)) (set! rax 1)))
  (check-paren-x64-fail `(begin (set! rax rbx)))
  (check-paren-x64-fail `(begin (set! rax (rbp - 0))))
  (check-paren-x64-fail
    `(begin
      (set! (rbp - 0) 10)
      (set! (rbp - 0) (rbp - 0))
      (set! rax 1)))
  (check-paren-x64-fail
    `(begin
      (set! (rbp - 0) 10)
      (set! (rbp - 0) (+ (rbp - 0) 10))
      (set! rax 1)))
  (check-paren-x64-fail
    `(begin
      (set! rax 10)
      (set! rax (+ rax ,(+ (max-int 32) 1)))))
  (check-paren-x64-fail
    `(begin
      (set! rax 10)
      (set! rax (+ rax rbx))))
  (check-paren-x64-fail
    `(begin
      (set! rax 10)
      (set! rbx 11)
      (set! rax (* rbx rax))))
  (check-paren-x64-fail
    `(begin
      (set! rax 10)
      (set! (rax - 0) 10)))
  (check-paren-x64-fail
    `(begin
      (set! rax 10)
      (set! (rbp - ,(* (+ (quotient (max-int 32) 8) 1) 8)) 10)))
  
  (check-paren-x64-success '(begin (set! rax 1)))
  (check-paren-x64-success '(begin (set! rax rbp)))
  (check-paren-x64-success
    `(begin
      (set! rax 10)
      (set! (rbp - ,(* (quotient (max-int 32) 8) 8)) 10)))
  (check-paren-x64-success
    `(begin
      (set! rax 1)
      (set! rbx ,(max-int 64))
      (set! rcx 900)
      (set! (rbp - 0) ,(max-int 32))
      (set! (rbp - 16) rcx)
      (set! rdx (rbp - 16))
      (set! rax (+ rax ,(max-int 32)))
      (set! rcx (* rcx rax))
      (set! rcx (* rcx (rbp - 0))))))
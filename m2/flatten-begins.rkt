#lang racket

(require cpsc411/langs/v2)

(provide flatten-begins)

;; Milestone 2 Exercise 9
;;
;; Flatten all nested begin expressions.
(define/contract (flatten-begins p)
  (-> nested-asm-lang-v2? para-asm-lang-v2?)

  ;; Removes nested begins from an effect.
  ;; effect -> (list effect ...)
  (define (flatten-begins-effect e)
    (match e
      [`(set! ,_ (,_ ,_ ,_)) (list e)]
      [`(set! ,_ ,_) (list e)]
      [`(begin ,es ... ,effect) 
        (append
          (append-map flatten-begins-effect es)
          (flatten-begins-effect effect))]))

  ;; Remove nested begins from a tail.
  ;; tail -> (list tail ...)
  (define (flatten-begins-tail t)
    (match t
      [`(begin ,es ... ,tail)
        (append
         (append-map flatten-begins-effect es)
         (flatten-begins-tail tail))]
      [`(halt ,t) `((halt ,t))]))

  `(begin ,@(flatten-begins-tail p)))

(module+ test
  (require rackunit)

  (define (check-42 p want)
    (define out (flatten-begins p))
    (check-equal? out want)
    (check-equal?
      (interp-para-asm-lang-v2 out)
      42))

  (check-42
    (flatten-begins '(halt 42))
    '(begin (halt 42)))
  ;; Interpreter doesn't seem to catch some errors...
  ;; It interperts just '(halt 42)...
  (check-equal?
    (flatten-begins '(halt 42))
    '(begin (halt 42)))


  (check-42
    (flatten-begins '(begin (halt 42)))
    '(begin (halt 42)))

  ; 1 level of nesting
  (check-42
    (flatten-begins
      '(begin
         (begin
           (set! rdi 1)
           (set! rdi (+ rdi 1)))
         (begin
           (set! rsi 0))
         (halt 42)))
    '(begin
       (set! rdi 1)
       (set! rdi (+ rdi 1))
       (set! rsi 0)
       (halt 42)))

  ; 2 levels of nesting
  (check-42
    (flatten-begins
      '(begin
         (begin
           (set! rdi 1)
           (begin
             (set! rbx 2)))
         (begin
           (set! rsi 0))
         (begin
           (halt 42))))
    '(begin
       (set! rdi 1)
       (set! rbx 2)
       (set! rsi 0)
       (halt 42))))

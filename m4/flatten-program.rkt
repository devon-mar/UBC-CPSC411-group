#lang racket

(require cpsc411/langs/v4)

(provide flatten-program)

;; Milestone 3 Exercise 6
;; Compile Block-asm-lang v4 to Para-asm-lang v4
;; by flattening basic blocks into labeled instructions.
(define/contract (flatten-program p)
  (-> block-asm-lang-v4? para-asm-lang-v4?)

  ;; (Block-Asm-Lang-v4 effect) -> (Para-Asm-Lang-v4 s)
  (define (flatten-effect e)
    (match e
      [`(set! ,_ ,_) e]
      [`(set! ,_ (,_ ,_ ,_)) e]))

  ;; (Block-Asm-Lang-v4 tail) -> (List-of (Para-Asm-Lang-v4 s))
  (define (flatten-tail t)
    (match t
      [`(halt ,_) (list t)]
      [`(jump ,_) (list t)]
      [`(begin ,effect ... ,tail)
       (append (map flatten-effect effect) (flatten-tail tail))]
      [`(if (,relop ,loc ,opand) (jump ,trg1) (jump ,trg2))
       `((compare ,loc ,opand)
         (jump-if ,relop ,trg1)
         (jump ,trg2))]))

  ;; (Block-Asm-Lang-v4 b) -> (List-of (Para-Asm-Lang-v4 s))
  (define (flatten-b b)
    (match b
      [`(define ,label ,tail)
       (define ts (flatten-tail tail))
       (define wl `(with-label ,label ,(first ts)))
       (append (list wl) (rest ts))]))

  (match p
    [`(module ,b ...)
     `(begin ,@(apply append (map flatten-b b)))]))

(module+ test
  (require rackunit)

  ;; base cases
  (check-equal?
    (flatten-program
      '(module (define L.test.1 (halt 0))))
    '(begin (with-label L.test.1 (halt 0))))
  
  ;; nested begins
  (check-equal?
    (flatten-program
      '(module
        (define L.test.1
          (begin
            (set! rdx 14)
            (set! rcx 9)
            (begin
              (set! rdx (* rdx 3))
              (set! fv0 rcx)
              (set! fv0 (+ fv0 rdx))
              (halt fv0))))))
    '(begin
      (with-label L.test.1 (set! rdx 14))
      (set! rcx 9)
      (set! rdx (* rdx 3))
      (set! fv0 rcx)
      (set! fv0 (+ fv0 rdx))
      (halt fv0)))
  
  ;; basic if
  (check-equal?
    (flatten-program
      '(module
        (define L.test.1
          (begin
            (set! rsi 9)
            (if (= rsi 9) (jump L.test.1) (jump done))))))
    '(begin
      (with-label L.test.1 (set! rsi 9))
      (compare rsi 9)
      (jump-if = L.test.1)
      (jump done)))

  ;; multiple defines w/ jumps and ifs
  (check-equal?
    (flatten-program
      '(module
        (define L.test.1 (jump L.test.3))
        (define L.test.2
          (begin
            (set! rdx L.test.4)
            (jump rdx)))
        (define L.test.3
          (begin
            (set! rdx 144)
            (set! r9 10)
            (set! fv0 L.test.2)
            (if (< r9 4) (jump fv0) (jump L.test.5))))
        (define L.test.4 (halt 5))
        (define L.test.5 (if (= rdx r9) (jump L.test.4) (jump L.test.2)))))
    '(begin
      (with-label L.test.1 (jump L.test.3))
      (with-label L.test.2 (set! rdx L.test.4))
      (jump rdx)
      (with-label L.test.3 (set! rdx 144))
      (set! r9 10)
      (set! fv0 L.test.2)
      (compare r9 4)
      (jump-if < fv0)
      (jump L.test.5)
      (with-label L.test.4 (halt 5))
      (with-label L.test.5 (compare rdx r9))
      (jump-if = L.test.4)
      (jump L.test.2)))
  )
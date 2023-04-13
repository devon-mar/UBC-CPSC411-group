#lang racket

(require cpsc411/langs/v8)

(provide flatten-program)

;; Milestone 4 Exercise 6
;; Milestone 7 Exercise 7
;; Milestone 8 Exercise 11
;;
;; Compile Block-asm-lang to Para-asm-lang
;; by flattening basic blocks into labeled instructions.
(define/contract (flatten-program p)
  (-> block-asm-lang-v8? para-asm-lang-v8?)

  ;; Unused
  #;
  (define (flatten-s s)
    (match s
      [`(set! ,loc_1 (mref ,loc_2 ,index)) s]
      [`(set! ,loc_1 (,binop ,loc_1 ,opand)) s]
      [`(set! ,loc ,triv) s]
      [`(mset! ,loc ,index ,triv) s]))

  ;; (Block-Asm-Lang-v8 tail) -> (List-of (Para-Asm-Lang-v8 s))
  (define (flatten-tail t)
    (match t
      [`(jump ,_) (list t)]
      [`(begin ,s ... ,tail)
       (append s (flatten-tail tail))]
      [`(if (,relop ,loc ,opand) (jump ,trg1) (jump ,trg2))
       `((compare ,loc ,opand)
         (jump-if ,relop ,trg1)
         (jump ,trg2))]))

  ;; (Block-Asm-Lang-v8 b) -> (List-of (Para-Asm-Lang-v8 s))
  (define (flatten-b b)
    (match b
      [`(define ,label ,tail)
       (define ts (flatten-tail tail))
       (define wl `(with-label ,label ,(first ts)))
       (append (list wl) (rest ts))]))

  (match p
    [`(module ,b ...)
     `(begin ,@(append-map flatten-b b))]))

(module+ test
  (require rackunit)

  ;; base cases
  (check-equal?
    (flatten-program
      '(module (define L.test.1 (jump done))))
    '(begin (with-label L.test.1 (jump done))))

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
              (set! (rbp - 0) rcx)
              (set! (rbp - 0) (+ (rbp - 0) rdx))
              (set! rax (rbp - 0))
              (jump done))))))
    '(begin
      (with-label L.test.1 (set! rdx 14))
      (set! rcx 9)
      (set! rdx (* rdx 3))
      (set! (rbp - 0) rcx)
      (set! (rbp - 0) (+ (rbp - 0) rdx))
      (set! rax (rbp - 0))
      (jump done)))

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
            (set! (rbp - 0) L.test.2)
            (if (< r9 4) (jump (rbp - 0)) (jump L.test.5))))
        (define L.test.4 (jump done))
        (define L.test.5 (if (= rdx r9) (jump L.test.4) (jump L.test.2)))))
    '(begin
      (with-label L.test.1 (jump L.test.3))
      (with-label L.test.2 (set! rdx L.test.4))
      (jump rdx)
      (with-label L.test.3 (set! rdx 144))
      (set! r9 10)
      (set! (rbp - 0) L.test.2)
      (compare r9 4)
      (jump-if < (rbp - 0))
      (jump L.test.5)
      (with-label L.test.4 (jump done))
      (with-label L.test.5 (compare rdx r9))
      (jump-if = L.test.4)
      (jump L.test.2)))

  ;; Check that the compiled program interprets to 42
  (define-check (check-42 p)
    (check-equal?
      (interp-para-asm-lang-v8 (flatten-program p))
      42))

  ;; mref, mset!
  (check-42
    '(module
      (define L.main.1
        (begin
          (mset! r12 16 6)
          (set! rdx (mref r12 16))
          (jump L.test.1)))
      (define L.test.1
        (begin
          (set! rdx (* rdx 7))
          (mset! r12 16 rdx)
          (set! rax (mref r12 16))
          (jump done)))))
  )

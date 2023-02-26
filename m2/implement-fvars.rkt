#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

(provide implement-fvars)

;; Milestone 2 Exercise 11
;; Milestone 4 Exercise 4
;;
;; Compiles the Paren-x64-fvars v4 to Paren-x64 v4 by reifying fvars into
;; displacement mode operands. The pass should use
;; current-frame-base-pointer-register.
(define/contract (implement-fvars p)
  (-> paren-x64-fvars-v4? paren-x64-v4?)

  (define/contract (loc? l)
    (-> any/c boolean?)
    (or (register? l) (fvar? l)))
  
  (define/contract (trg? l)
    (-> any/c boolean?)
    (or (register? l) (label? l)))

  ;; Return the displacement mode operand for fvar f.
  (define/contract (implement-fvars-fvar f)
    (-> fvar? any/c)
    `(,(current-frame-base-pointer-register) - ,(* 8 (fvar->index f))))


  ;; Replace fvars with displacement mode operands in l.
  ;;
  ;; paren-x64-fvars-v4-loc -> paren-x64-v4-loc
  (define (implement-fvars-loc l)
    (match l
      [reg #:when (register? reg) reg]
      [fvar (implement-fvars-fvar fvar)]))

  ;; Replace fvars with displacement mode operands in s.
  ;;
  ;; paren-x64-fvars-v4-s -> paren-x64-v4-s
  (define (implement-fvars-s s)
    (match s
      [`(set! ,_ (,_ ,_ ,int32))
        #:when (int32? int32)
        s]
      [`(set! ,reg (,binop ,reg ,loc))
        `(set! ,reg (,binop ,reg ,(implement-fvars-loc loc)))]
      [`(set! ,fvar ,int32)
        #:when (and (fvar? fvar) (int32? int32))
        `(set! ,(implement-fvars-fvar fvar) ,int32)]
      [`(set! ,fvar ,trg)
        #:when (and (fvar? fvar) (trg? trg))
        `(set! ,(implement-fvars-fvar fvar) ,trg)]
      [`(set! ,reg ,loc)
        #:when (and (register? reg) (loc? loc))
        `(set! ,reg ,(implement-fvars-loc loc))]
      [`(set! ,_ ,_)
        s]
      [`(with-label ,_ ,_)
        s]
      [`(jump ,_)
        s]
      [`(compare ,_ ,_)
        s]
      [`(jump-if ,_ ,_)
        s]))

  ;; Replace fvars with displacement mode operands in p.
  ;;
  ;; paren-x64-fvars-v4-p -> paren-x64-v4-p
  (define (implement-fvars-p p)
    (match p
      [`(begin ,s ...) `(begin ,@(map implement-fvars-s s))]))

  (implement-fvars-p p))

(module+ test
  (require rackunit)
  (check-equal? (implement-fvars '(begin)) '(begin))
  (check-equal? (implement-fvars '(begin (set! rax 0))) '(begin (set! rax 0)))

  (check-match
    (implement-fvars
      `(begin
         ; (set! fvar int32)
         (set! fv0 2)
         ; (set! reg loc)
         (set! rsi fv0)
         ; (set! reg triv)
         (set! rdi ,(add1 (max-int 32)))
         ; (set! reg_1 (binop reg_1 int32))
         (set! rsi (+ rsi 40)) 
         ; (set! fvar reg)
         (set! fv1 rax)
         ; (set! reg_1 (binop reg_1 loc))
         (set! rsi (+ rsi fv0))
         (set! rax rsi)))
    `(begin
       (set! (,rbp - 0) 2)
       (set! rsi (,rbp - 0))
       (set! rdi ,val1)
       (set! rsi (+ rsi 40))
       (set! (,rbp - 8) rax)
       (set! rsi (+ rsi (,rbp - 0)))
       (set! rax rsi))
  (and
    (equal? rbp (current-frame-base-pointer-register))
    (equal? val1 (add1 (max-int 32)))))
  
  ; control flow
  (check-equal?
    (implement-fvars
      '(begin
        (set! fv0 L.$!!@#*main.2)
        (with-label L.$!!@#*main.2 (with-label L.test.3 (jump rax)))
        (jump L.test.4)
        (compare rax rbx)
        (jump-if <= L.test!!.5)))
    `(begin
      (set! (,(current-frame-base-pointer-register) - 0) L.$!!@#*main.2)
      (with-label L.$!!@#*main.2 (with-label L.test.3 (jump rax)))
      (jump L.test.4)
      (compare rax rbx)
      (jump-if <= L.test!!.5)))
  )

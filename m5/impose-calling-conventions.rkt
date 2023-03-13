#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5)

(provide impose-calling-conventions)

;; Milestone 5 Exercise 5
;;
;; Compiles Proc-imp-cmf-lang v5 to Imp-cmf-lang v5 by imposing calling
;; conventions on all calls and procedure definitions. The parameter registers
;; are defined by the list current-parameter-registers.
(define/contract (impose-calling-conventions p)
  (-> proc-imp-cmf-lang-v5? imp-cmf-lang-v5?)

  ;; Maps each paramameter to a loc. All frame variable locs (if any) will be
  ;; at the end of the list.
  ;;
  ;; (listof proc-imp-cmf-lang-v5-aloc) -> (dict proc-imp-cmf-lang-v5-aloc -> imp-cmf-lang-v5-loc)
  (define/contract (params->locs p)
    (-> (listof aloc?) dict?)
    (let f ([p p]
            [next (current-parameter-registers)]
            [acc '()])
      (cond
        [(empty? p) acc]
        [(empty? next)
         (f 
           (cdr p)
           1
           (dict-set acc (car p) (make-fvar 0)))]
        [(number? next)
         (f 
           (cdr p)
           (add1 next)
           (dict-set acc (car p) (make-fvar next)))]
        [(f
           (cdr p)
           (cdr next)
           (dict-set acc (car p) (car next)))])))

  ;; Imposes calling conventions on a procedure with label `label`,
  ;; params `params`, and tail `tail`. Returns the new procedure.
  ;;
  ;; tail: proc-imp-cmf-lang-v5-tail
  ;; -> imp-cmf-lang-v5-tail
  (define/contract (impose-calling-conventions-proc label params tail)
    (-> label? (listof aloc?) any/c any/c)
    (define locs (params->locs params))
    `(define
       ,label
       ,(make-begin
          (map (lambda (p) `(set! ,p ,(dict-ref locs p))) params)
          (impose-calling-conventions-tail tail))))

  ;;
  ;; All frame varibale locs (if any) will be at the end of the list.
  ;;
  ;; cnt: nubmer of args
  ;; -> (listof imp-cmf-lang-v5-loc)
  (define/contract (args->locs cnt)
    (-> exact-nonnegative-integer? (listof (or/c register? fvar? aloc?)))
    (let f ([cnt cnt]
            [next (current-parameter-registers)])
      (cond
        [(zero? cnt) '()]
        [(empty? next)
         (cons
           (make-fvar 0)
           (f (sub1 cnt) 1))]
        [(number? next)
         (cons
           (make-fvar next)
           (f (sub1 cnt) (add1 next)))]
        [(cons
           (car next)
           (f (sub1 cnt) (cdr next)))])))

  ;; proc-imp-cmf-lang-v5-p -> imp-cmf-lang-v5-p
  (define (impose-calling-conventions-p p)
    (match p
      [`(module (define ,labels (lambda (,alocs ...) ,tails)) ... ,tail)
        `(module
           ,@(map impose-calling-conventions-proc labels alocs tails)
           ,(impose-calling-conventions-tail tail))]))

  ;; proc-imp-cmf-lang-v5-pred -> imp-cmf-lang-v5-pred
  (define (impose-calling-conventions-pred p)
    (match p
      [`(true)
        p]
      [`(false)
        p]
      [`(not ,p)
        `(not ,(impose-calling-conventions-pred p))]
      [`(begin ,effects ... ,pred)
        `(begin
           ,@(map impose-calling-conventions-effect effects)
           ,(impose-calling-conventions-pred pred))]
      [`(if ,p1 ,p2 ,p3)
        `(if
           ,(impose-calling-conventions-pred p1)
           ,(impose-calling-conventions-pred p2)
           ,(impose-calling-conventions-pred p3))]
      [`(,relop ,o1 ,o2)
        `(,relop ,o1 ,o2)]))

  ;; proc-imp-cmf-lang-v5-tail -> imp-cmf-lang-v5-tail
  (define (impose-calling-conventions-tail t)
    (match t
      [`(call ,triv ,os ...)
        (define locs (args->locs (length os)))
        `(begin
           ,@(reverse (map (lambda (a l) `(set! ,l ,a)) os locs))
           (jump
             ,triv
             ,(current-frame-base-pointer-register) ,@locs))]
      [`(begin ,es ... ,t)
        `(begin
           ,@(map impose-calling-conventions-effect es)
           ,(impose-calling-conventions-tail t))]
      [`(if ,p ,t1 ,t2)
        `(if
           ,(impose-calling-conventions-pred p)
           ,(impose-calling-conventions-tail t1)
           ,(impose-calling-conventions-tail t2))]
      ;; value
      [_ t]))


  ;; proc-imp-cmf-lang-v5-effect -> imp-cmf-lang-v5-effect
  (define (impose-calling-conventions-effect e)
    (match e
      [`(set! ,_ ,_)
        e]
      ;; Modified template - removed tail e since we assume valid input.
      ;; ...well it has to be since we use define/contract.
      [`(begin ,es ...)
        `(begin
           ,@(map impose-calling-conventions-effect es))]
      [`(if ,p ,e1 ,e2)
        `(if
           ,(impose-calling-conventions-pred p)
           ,(impose-calling-conventions-effect e1)
           ,(impose-calling-conventions-effect e2))]))

  ;; not used
  #;
  (define (impose-calling-conventions-value v)
    (match v
      [`(,b ,o1 ,o2)
        (void)]
      [triv (void)]))

  ;; not used
  #;
  (define (impose-calling-conventions-opand o)
    (match o
      [(? int64?)
       (void)]
      [(? aloc?)
       (void)]))

  ;; not used
  #;
  (define (impose-calling-conventions-triv t)
    (match t
      [(? label?)
       (void)]
      [opand
       (void)]))

  ;; not used
  #;
  (define (impose-calling-conventions-binop b)
    (match b
      ['* (void)]
      ['+ (void)]))

  ;; not used
  #;
  (define (impose-calling-conventions-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))


  (impose-calling-conventions-p p))

(module+ test
  (require rackunit)

  (define-check (check-42 p)
    (check-equal?
      (interp-imp-cmf-lang-v5 (impose-calling-conventions p))
      42))

  ;; tail/value
  (check-42 '(module 42))
  (check-42 '(module (define L.foo.1 (lambda () 42)) (call L.foo.1)))
  (check-42 '(module (define L.foo.1 (lambda (x.1) x.1)) (call L.foo.1 42)))

  (define prog1
    '(module (define L.foo.1 (lambda (x.1 y.1) (+ x.1 y.1))) (call L.foo.1 40 2)))
  (check-42 prog1)
  ;; Same as above, but to make sure it actually doing the right things
  (check-equal?
    (impose-calling-conventions prog1)
    `(module
       (define L.foo.1
         (begin
           (set! x.1 ,(first (current-parameter-registers)))
           (set! y.1 ,(second (current-parameter-registers)))
           (+ x.1 y.1)))
       (begin
         (set! ,(second (current-parameter-registers)) 2)
         (set! ,(first (current-parameter-registers)) 40)
         (jump
           L.foo.1
           ,(current-frame-base-pointer-register)
           ,(first (current-parameter-registers))
           ,(second (current-parameter-registers))))))

  (define prog2
    '(module
       (define L.foo.1
         (lambda (a.0 a.1 a.2 a.3 a.4 a.5 a.6 a.7 a.8 a.9)
           (begin
             (set! a.0 (+ a.0 a.1))
             (set! a.0 (+ a.0 a.2))
             (set! a.0 (+ a.0 a.3))
             (set! a.0 (+ a.0 a.4))
             (set! a.0 (+ a.0 a.5))
             (set! a.0 (+ a.0 a.6))
             (set! a.0 (+ a.0 a.7))
             (set! a.0 (+ a.0 a.8))
             (set! a.0 (+ a.0 a.9))
             a.0)))
       (call L.foo.1 1 2 3 4 5 6 7 8 9 -3)))
  (check-42 prog2)
  ;; Same as above, but to make sure it actually doing the right things
  (check-equal?
    (impose-calling-conventions prog2)
    `(module
       (define L.foo.1
         (begin
           (set! a.0 ,(first (current-parameter-registers)))
           (set! a.1 ,(second (current-parameter-registers)))
           (set! a.2 ,(third (current-parameter-registers)))
           (set! a.3 ,(fourth (current-parameter-registers)))
           (set! a.4 ,(fifth (current-parameter-registers)))
           (set! a.5 ,(sixth (current-parameter-registers)))
           (set! a.6 ,(make-fvar 0))
           (set! a.7 ,(make-fvar 1))
           (set! a.8 ,(make-fvar 2))
           (set! a.9 ,(make-fvar 3))
           (set! a.0 (+ a.0 a.1))
           (set! a.0 (+ a.0 a.2))
           (set! a.0 (+ a.0 a.3))
           (set! a.0 (+ a.0 a.4))
           (set! a.0 (+ a.0 a.5))
           (set! a.0 (+ a.0 a.6))
           (set! a.0 (+ a.0 a.7))
           (set! a.0 (+ a.0 a.8))
           (set! a.0 (+ a.0 a.9))
           a.0))
       (begin
         (set! ,(make-fvar 3) -3)
         (set! ,(make-fvar 2) 9)
         (set! ,(make-fvar 1) 8)
         (set! ,(make-fvar 0) 7)
         (set! ,(sixth (current-parameter-registers)) 6)
         (set! ,(fifth (current-parameter-registers)) 5)
         (set! ,(fourth (current-parameter-registers)) 4)
         (set! ,(third (current-parameter-registers)) 3)
         (set! ,(second (current-parameter-registers)) 2)
         (set! ,(first (current-parameter-registers)) 1)
         (jump
           L.foo.1
           ,(current-frame-base-pointer-register)
           ,@(current-parameter-registers)
           ,(make-fvar 0)
           ,(make-fvar 1)
           ,(make-fvar 2)
           ,(make-fvar 3)))))


  (check-42
    '(module
       (define L.foo.1
         (lambda (x.1 y.1) (+ x.1 y.1)))
       (define L.bar.1
         (lambda (x.1 y.1) (* x.1 y.1)))
       (define L.baz.1
         (lambda (x.1 y.1) z.1))
       ;; tail/(if pred tail tail)
       (if
         ;; pred/(relop opand opand)
         (= 10 10)
         ;; pred/(false)
         (if (false)
           (call L.baz.1 0 0)
           (call L.foo.1 21 21))
         (call L.bar.1 0 0))))

  ;; buf
  ;; tail/(call triv/opand/int64 opand ...)

  (check-42
    '(module
       (define L.foo.1
         (lambda (x.1 x.2 x.3 x.4 x.5 x.6 x.7) x.7))
       ;; tail/(begin effect ... tail)
       (begin
         ;; pred/(true)
         ;; effect/(if pred effect effect)
         (if (true)
           ;; effect/(set! aloc value)
           (set! x.1 L.foo.1)
           (set! x.1 10))
         ;; tail/(call triv/opand/aloc opand ...)
         (call x.1 0 0 0 0 0 0 42))))

  (check-42
    '(module
       (define L.foo.1
         (lambda (z.1)
           (begin
             (set! x.1 1)
             (set! x.1 (+ x.1 10))
             (+ x.1 z.1))))
       (begin
         ;; effect/(begin effect ... effect)
         (begin
           (set! x.1 29)
           (set! x.2 2))
         ;; value/(binop opand opand)
         (set! x.2 (+ x.2 x.1))
         ;; tail/(call triv/label opand ...)
         (call L.foo.1 x.2))))

  (check-42
    '(module
       (define L.foo.1
         (lambda (x.1 y.1 z.1)
           ;; pred/(begin effect ... pred)
           (if (begin
                 (set! a.1 x.1)
                 (set! b.1 y.1)
                 ;; pred/(if pred pred pred)
                 (if (true)
                   ;; pred/(not pred)
                   (not (> a.1 b.1))
                   (false)))
             42
             z.1)))
       (call L.foo.1 10 20 3)))
  )

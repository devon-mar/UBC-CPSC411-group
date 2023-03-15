#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5)

(provide sequentialize-let)

;; Milestone 2 Exercise 2
;; Milestone 4 Exercise 18
;; Milestone 5 Exercise 3
;;
;; Compiles Values-unique-lang v5 to Imp-mf-lang v5 by picking a particular
;; order to implement let expressions using set!.
(define/contract (sequentialize-let p)
  (-> values-unique-lang-v5? imp-mf-lang-v5?)

  ;; Sequentialize a procedure with the given label, params and tail.
  ;;
  ;; tail: values-unique-lang-v5-tail
  ;; -> imp-mf-lang-v5-proc
  (define/contract (sequentialize-let-proc label params tail)
    (-> label? (listof aloc?) any/c any/c)
    `(define
       ,label
       (lambda ,params ,(sequentialize-let-tail tail))))

  ;; values-unique-lang-v5-p -> imp-lang-v5-p
  (define (sequentialize-let-p p)
    (match p
      [`(module (define ,labels (lambda (,alocs ...) ,tails)) ... ,tail)
        `(module
           ,@(map sequentialize-let-proc labels alocs tails)
           ,(sequentialize-let-tail tail))]))

  ;; map an aloc and value pair to a set!
  ;;
  ;; a: aloc
  ;; v: value
  ;; -> set statement
  (define/contract (aloc-value->set a v)
    (-> aloc? any/c list?)
    `(set! ,a ,(sequentialize-let-value v)))

  ;; values-unique-lang-v5-pred -> imp-lang-v5-pred
  (define (sequentialize-let-pred p)
    (match p
      [`(true)
        p]
      [`(false)
        p]
      [`(not ,p)
        `(not ,(sequentialize-let-pred p))]
      [`(let ([,as ,vs] ...) ,pred)
        `(begin
           ,@(map aloc-value->set as vs)
           ,(sequentialize-let-pred pred))]
      [`(if ,p1 ,p2 ,p3)
        `(if
           ,(sequentialize-let-pred p1)
           ,(sequentialize-let-pred p2)
           ,(sequentialize-let-pred p3))]
      [`(,_ ,_ ,_) p]))

  ;; values-unique-lang-v5-tail -> imp-lang-v5-tail
  (define (sequentialize-let-tail t)
    (match t
      [`(let ([,as ,vs] ...) ,tail)
        `(begin
          ,@(map aloc-value->set as vs)
          ,(sequentialize-let-tail tail))]
      [`(if ,p ,t1 ,t2)
        `(if
           ,(sequentialize-let-pred p)
           ,(sequentialize-let-tail t1)
           ,(sequentialize-let-tail t2))]
      [`(call ,_ ,_ ...) t]
      ;; value
      [_ (sequentialize-let-value t)]))

  ;; values-unique-lang-v5-value -> imp-lang-v5-value
  (define (sequentialize-let-value v)
    (match v
      [`(let ([,as ,vs] ...) ,v)
        `(begin
          ,@(map aloc-value->set as vs)
          ,(sequentialize-let-value v))]
      [`(if ,p ,v1 ,v2)
        `(if
           ,(sequentialize-let-pred p)
           ,(sequentialize-let-value v1)
           ,(sequentialize-let-value v2))]
      [`(,_ ,_ ,_) v]
      ;; triv
      [_ v]))

  ;; not used
  #;
  (define (sequentialize-let-opand o)
    (match o
      [(? int64?)
       (void)]
      [(? aloc?)
       (void)]))

  ;; not used
  #;
  (define (sequentialize-let-triv t)
    (match t
      [(? label?)
       (void)]
      [opand
       (void)]))

  ;; not used
  #;
  (define (sequentialize-let-binop b)
    (match b
      ['* (void)]
      ['+ (void)]))

  ;; not used
  #;
  (define (sequentialize-let-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (sequentialize-let-p p))

(module+ test
  (require rackunit)

  (define-check (check-42 p)
    (check-equal?
      (interp-values-unique-lang-v5 (sequentialize-let p))
      42))

  ;; new m3 stuff
  (check-42
    '(module
       (let ([x.1 1])
         (if (not (let ([x.2 (if (true) 2 1)]) (= x.2 2)))
           0
           (if (if (true) (false) (> 10 1))
             0
             42)))))


  ;; M2 tests
  ;; very simple
  (check-42 '(module 42))

  ;; simple
  (check-42 '(module (let ([x.3 42]) x.3)))

  ;; with binop
  (check-42 '(module (let ([x.4 (+ 40 2)]) x.4)))

  ;; let in tail of let
  (check-42 '(module (let ([x.7 2]) (let ([x.8 21]) (+ x.8 x.8)))))

  ;; nested let
  ;; bug??
  ;; should x.9 be accessible?
  ;; (interp '(module (let ([x.7 5] [x.8 (let ([x.9 40]) (+ x.9 2))]) x.9)))
  (check-42 '(module (let ([x.7 5] [x.8 (let ([x.9 40]) (+ x.9 2))]) x.8)))

  ;; M5 tests
  (check-42
    '(module
       (define L.foo.1
         (lambda (x.1)
           ;; tail/let
           (let ([z.1 x.1]
                 [y.1 2])
             (+ z.1 y.1))))
       (let ([a.1 2]
             [b.1 38])
         (let ([c.1 (+ a.1 b.1)])
           (call L.foo.1 c.1)))))

  (check-42
    '(module
       (define L.foo.1
         (lambda (x.1 y.1 z.1)
           ;; pred/let
           (if (let ([c.1 (+ x.1 y.1)]) (> z.1 c.1))
             z.1
             x.1)))
       (call L.foo.1 1 2 42)))

  ;; value/let
  (check-42
    '(module
       (define L.foo.1
         (lambda (x.1 y.1 z.1)
           (let ([b.1 (let ([a.1 (+ y.1 z.1)]) (+ a.1 x.1))])
             b.1)))
       (call L.foo.1 10 30 2)))

  ;; let in not
  (check-42
    '(module
       (define L.foo.1
         (lambda (x.1 y.1 z.1)
           (if (not (let ([c.1 (+ x.1 y.1)]) (> z.1 c.1)))
             x.1
             z.1)))
       (call L.foo.1 1 2 42)))
  )

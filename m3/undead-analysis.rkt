#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

(provide undead-analysis)

;; Milestone 3 - Exercise 1
;; Milestone 4 - Exercise 15

;; Performs undeadness analysis, decorating the program with undead-set tree.
;; Only the info field of the program is modified.
(define (undead-analysis p)
  (-> asm-pred-lang-v4/locals? asm-pred-lang-v4/undead?)

  ;; Performs undeadness analysis, decorating the program with undead-set tree.
  ;; Only the info field of the program is modified.
  (define (undead-analysis-p p)
    (-> asm-pred-lang-v4/locals? asm-pred-lang-v4/undead?)
    (match p
      [`(module ,info ,tail)
        `(module
          ,(info-set
             info
             'undead-out
             (let-values
               ([(ust _) (undead-analysis-tail tail '())])
               ust))
          ,tail)]))

  ;; Computes the undead-in set for instruction t
  ;; given the undead-out set for t.
  ;;
  ;; tail undead-set? -> (values undead-set-tree? undead-set?)
  (define (undead-analysis-tail t uo)
    (-> any/c undead-set? 
        (values undead-set-tree? undead-set?))

    (match t
      [`(halt ,triv)
        (values
          '()
          (undead-analysis-triv triv uo))]
      [`(begin ,effects ... ,tail)
        (define-values (tail-ust tail-ui)
          (undead-analysis-tail tail uo))

        (for/foldr
          ([acc-ust (list tail-ust)]
           [acc-ui tail-ui])
          ([e effects])
          (let-values
            ([(new-ust new-ui)
              (undead-analysis-effect e acc-ui)])
            (values
              (cons new-ust acc-ust)
              new-ui)))]
      [`(if ,pred ,tail1 ,tail2)
       (define-values (ut1 ui1) (undead-analysis-tail tail1 uo))
       (define-values (ut2 ui2) (undead-analysis-tail tail2 uo))
       (define-values (utp uip) (undead-analysis-pred pred (set-union ui1 ui2)))
       (values (list utp ut1 ut2) uip)]))

  ;; Computes the undead-in set for instruction e
  ;; given the undead-out set for e.
  ;;
  ;; effect undead-set? -> (values undead-set-tree? undead-set?)
  (define (undead-analysis-effect e uo)
    (-> any/c undead-set?
        (values undead-set-tree? undead-set?))

    (match e
      [`(set! ,aloc (,_binop ,aloc ,triv))
        ;; 1. define aloc -> set-remove
        ;; 2. reference aloc -> set-add
        ;; 3. reference triv -> undead-analysis-triv
        (values
          uo
          (undead-analysis-triv
            triv
            (set-add uo aloc)))]
      [`(set! ,aloc ,triv)
        (values
          uo
          (undead-analysis-triv
            triv
            (set-remove uo aloc)))]
      ;; NOTE: Removed 'tail' effect
      ;; since we assume valid input
      [`(begin ,effects ...)
        (for/foldr
          ([acc-ust '()]
           [acc-ui uo])
          ([e effects])
          (let-values
            ([(new-ust new-ui)
              (undead-analysis-effect e acc-ui)])
            (values
              (cons new-ust acc-ust)
              new-ui)))]
      [`(if ,pred ,effect1 ,effect2)
       (define-values (ut1 ui1) (undead-analysis-effect effect1 uo))
       (define-values (ut2 ui2) (undead-analysis-effect effect2 uo))
       (define-values (utp uip) (undead-analysis-pred pred (set-union ui1 ui2)))
       (values (list utp ut1 ut2) uip)]))
  
  ;; Computes undead set tree and undead-in set for pred p
  ;; given undead-out set for p
  (define (undead-analysis-pred p uo)
    (-> any/c undead-set? (values undead-set-tree? undead-set?))
    (match p
 	 	  [`(true) (values uo uo)]
 	 	  [`(false) (values uo uo)]
 	 	  [`(not ,pred) (undead-analysis-pred pred uo)]
 	 	  [`(begin ,effects ... ,pred)
       (define-values (pred-ust pred-ui)
         (undead-analysis-pred pred uo))
       (for/foldr
         ([acc-ust (list pred-ust)]
          [acc-ui pred-ui])
         ([e effects])
         (let-values
           ([(new-ust new-ui)
             (undead-analysis-effect e acc-ui)])
           (values (cons new-ust acc-ust) new-ui)))]
      [`(,_ ,aloc ,triv)
       (values uo (undead-analysis-triv triv (set-add uo aloc)))]
 	 	  [`(if ,ppred ,pred1 ,pred2)
       (define-values (ut1 ui1) (undead-analysis-pred pred1 uo))
       (define-values (ut2 ui2) (undead-analysis-pred pred2 uo))
       (define-values (utp uip) (undead-analysis-pred ppred (set-union ui1 ui2)))
       (values (list utp ut1 ut2) uip)]))

  ;; Returns uo with t if t is a triv AND t is in uo,
  ;; otherwise returns uo.
  ;;
  ;; t: triv
  ;; uo: undead-set?
  (define (undead-analysis-triv t uo)
    (-> any/c undead-set? undead-set?)
    (match t
      [(? int64?) uo]
      [(? aloc?) 
       (set-add uo t)]))

  ;; Don't need
  #;
  (define (undead-analysis-binop b uo)
    (match b
      ['* (void)]
      ['+ (void)]))
  (undead-analysis-p p))

(module+ test
  (require rackunit)

  ;; For CPSC411 test suite
  (require
   rackunit/text-ui
   cpsc411/test-suite/public/v2-reg-alloc)

  (define (set-equal? s1 s2)
    (-> set? set? boolean?)
    (empty? (set-symmetric-difference s1 s2)))

  (check-true (set-equal? '(1 2) '(1 2)))
  (check-true (set-equal? '(2 1) '(1 2)))
  (check-false (set-equal? '(2 1 3) '(1 2)))
  (check-false (set-equal? '(2 1) '(1 2 3)))

  ;; Quick helper to check that two undead-set-trees 
  ;; are equal.
  (define (ust-equal? have want)
    (-> undead-set-tree? undead-set-tree? boolean?)
    (match (cons have want)
      [(cons '() '()) #t]
      [(cons '() _) #f]
      [(cons _ '()) #f]
      [(cons s1 s2)
       #:when (aloc? (car s2))
       (set-equal? s1 s2)]
      [(cons s1 s2)
       (andmap ust-equal? s1 s2)]))
  (check-true (ust-equal? '() '()))
  (check-true (ust-equal? '(x.1) '(x.1)))
  (check-true (ust-equal? '((x.1 x.2) (x.1)) '((x.1 x.2) (x.1))))
  (check-true (ust-equal? '((x.2 x.1) (x.1)) '((x.1 x.2) (x.1))))
  (check-true (ust-equal? '((((x.2 x.1) (x.1)))) '((((x.2 x.1) (x.1))))))

  ;; Checks that the ust produced by `undead-analysis` is
  ;; equal to want. Also checks that the rest of the program
  ;; is the same.
  ;;
  ;; locals: (aloc ...)
  ;; tail: tail
  ;; want: the expected locals
  (define (check-ust locals tail want)
    (match (undead-analysis `(module ([locals ,locals]) ,tail))
      [`(module ,info ,tail2)
        ;; Should just contain locals and undead-out
        (check-equal?
          (length info)
          2)
        (check
          ust-equal?
          (info-ref info 'undead-out)
          want
          "undead-out not equal")
        ;; Should remain unchanged
        (check-equal? tail2 tail)]))

  ;; Simple
  (check-ust
    '()
    '(halt 42)
    '())

  (check-ust
    '()
    '(begin (halt 42))
    '(()))

  ;; Unused set!
  (check-ust
    '(x.1)
    '(begin
       (set! x.1 42)
       (halt 42))
    '(()
      ()))

  ;; Referenced set!
  (check-ust
    '(x.1)
    '(begin
       (set! x.1 42)
       (halt x.1))
    '((x.1)
      ()))

  ;; Referenced binop
  (check-ust
    '(x.1)
    '(begin
       (set! x.1 40)
       (set! x.1 (+ x.1 2))
       (halt x.1))
    '((x.1)
      (x.1)
      ()))

  ;; Unreferenced binop
  (check-ust
    '(x.1)
    '(begin
       (set! x.1 40)
       (set! x.1 (+ x.1 2))
       (halt 42))
    '((x.1)
      ()
      ()))

  ;; binop lhs aloc
  (check-ust
    '(x.1 x.2)
    '(begin
       (set! x.2 2)
       (set! x.1 40)
       (set! x.1 (+ x.1 x.2))
       (halt x.1))
    '((x.2)
      (x.2 x.1)
      (x.1)
      ()))

  ;; nested begin in tail-tail
  (check-ust
    '(x.1 x.2)
    '(begin
       (set! x.1 40)
       (begin
         (set! x.2 2)
         (set! x.1 (+ x.1 x.2))
         (halt x.1)))
    '((x.1)
      ((x.2 x.1)
       (x.1)
       ())))

  ;; nested begin in tail-effect
  (check-ust
    '(x.1 x.2)
    '(begin
       (set! x.1 40)
       (begin
         (set! x.2 2)
         (set! x.1 (+ x.1 x.2)))
       (halt x.1))
    '((x.1)
      ((x.2 x.1)
       (x.1))
      ()))

  ;; from book 1
  (check-ust
    '(x.1)
    '(begin
       (set! x.1 42)
       (halt x.1))
    '((x.1)
      ()))

  ;; from book 2
  (check-ust
    '(v.1 w.2 x.3 y.4 z.5 t.6 p.1)
    '(begin
       (set! v.1 1)
       (set! w.2 46)
       (set! x.3 v.1)
       (set! p.1 7)
       (set! x.3 (+ x.3 p.1))
       (set! y.4 x.3)
       (set! p.1 4)
       (set! y.4 (+ y.4 p.1))
       (set! z.5 x.3)
       (set! z.5 (+ z.5 w.2))
       (set! t.6 y.4)
       (set! p.1 -1)
       (set! t.6 (* t.6 p.1))
       (set! z.5 (+ z.5 t.6))
       (halt z.5))

    '((v.1)
      (v.1 w.2)
      (x.3 w.2)
      (p.1 x.3 w.2)
      (x.3 w.2)
      (y.4 x.3 w.2)
      (p.1 y.4 x.3 w.2)
      (x.3 w.2 y.4)
      (w.2 z.5 y.4)
      (y.4 z.5)
      (t.6 z.5)
      (p.1 t.6 z.5)
      (t.6 z.5)
      (z.5)
      ()))

  ;; if in tail
  (check-ust
    '(v.0 x.1 y.2 z.3 i.1 a.1 b.1 c.1)
    '(begin
      (set! v.0 4)
      (set! x.1 5)
      (set! y.2 6)
      (set! z.3 7)
      (if (begin (set! i.1 10) (not (= y.2 5)))
          (begin (set! a.1 i.1) (set! a.1 (* a.1 9)) (halt v.0))
          (begin (set! b.1 x.1) (set! c.1 b.1) (halt 10))))
    '((v.0)
      (x.1 v.0)
      (x.1 y.2 v.0)
      (x.1 y.2 v.0)
      (((x.1 i.1 v.0 y.2) (x.1 i.1 v.0))
       ((v.0 a.1) (v.0) ())
       ((b.1) () ()))))

  ;; if in effect
  (check-ust
    '(u.1 v.1 x.1 y.1 z.1 i.1 a.1 b.1 c.1 d.1)
    '(begin
      (set! u.1 4)
      (set! v.1 4)
      (set! x.1 5)
      (if (not (false)) (set! v.1 9) (set! a.1 u.1))
      (set! y.1 6)
      (set! z.1 7)
      (if (begin (set! i.1 10) (< y.1 5))
          (begin (set! a.1 i.1) (set! u.1 9) (set! a.1 (* a.1 9)) (set! v.1 9))
          (begin (set! b.1 x.1) (set! c.1 b.1) (set! u.1 10)))
      (set! d.1 u.1)
      (halt v.1))
    '((u.1)
      (v.1 u.1)
      (v.1 x.1 u.1)
      ((v.1 x.1 u.1) (v.1 x.1) (v.1 x.1))
      (v.1 x.1 y.1)
      (v.1 x.1 y.1)
      (((i.1 v.1 x.1 y.1) (i.1 v.1 x.1))
       ((a.1) (u.1 a.1) (u.1) (v.1 u.1))
       ((v.1 b.1) (v.1) (v.1 u.1)))
      (v.1)
      ()))

  ;; if in pred
  (check-ust
    '(u.1 v.1 x.1 y.1 z.1 i.1 a.1 b.1 c.1)
    '(begin
      (set! u.1 4)
      (set! v.1 4)
      (set! x.1 5)
      (set! y.1 6)
      (set! z.1 7)
      (if (begin
            (set! i.1 0)
            (if (true)
                (begin (set! a.1 i.1) (set! u.1 9) (set! a.1 (* a.1 9)) (< v.1 9))
                (begin (set! b.1 x.1) (set! c.1 b.1) (= c.1 10))))
          (halt u.1)
          (halt z.1)))
    '((u.1)
      (v.1 u.1)
      (v.1 u.1 x.1)
      (v.1 u.1 x.1)
      (z.1 v.1 u.1 x.1)
      (((z.1 v.1 i.1 u.1 x.1)
        ((z.1 v.1 i.1 u.1 x.1)
         ((z.1 v.1 a.1) (u.1 z.1 v.1 a.1) (u.1 z.1 v.1) (u.1 z.1))
         ((u.1 z.1 b.1) (u.1 z.1 c.1) (u.1 z.1))))
       ()
       ())))
  )

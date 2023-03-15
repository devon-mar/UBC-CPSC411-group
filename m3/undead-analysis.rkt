#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5
  "../utils/compiler-utils.rkt")

(provide undead-analysis)

;; Milestone 3 - Exercise 1
;; Milestone 4 - Exercise 15
;; Milestone 5 - Exercise 8
;;
;; Performs undeadness analysis, decorating the program with undead-set tree.
;; Only the info field of the program is modified.
(define/contract (undead-analysis p)
  (-> asm-pred-lang-v5/locals? asm-pred-lang-v5/undead?)

  ;; Performs undeadness analysis, decorating the program with undead-set tree.
  ;; Only the info field of the program is modified.
  (define/contract (undead-analysis-p p)
    (-> asm-pred-lang-v5/locals? asm-pred-lang-v5/undead?)
    (match p
      [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
        `(module
          ,(info-set
             info
             'undead-out
             (let-values
               ([(ust _) (undead-analysis-tail tail '())])
               ust))
          ,@(map undead-analysis-proc labels infos tails)
          ,tail)]))

  ;; Performs undead analysis and adds undead-set tree to info of proc
  ;; proc ::= (define label info tail)
  ;;
  ;; label info tail -> proc
  (define/contract (undead-analysis-proc label info tail)
    (-> label? info? any/c any/c)
    (define-values (ust _) (undead-analysis-tail tail '()))
    `(define ,label ,(info-set info 'undead-out ust) ,tail))

  ;; Computes the undead-in set for instruction t
  ;; given the undead-out set for t.
  ;;
  ;; tail undead-set/rloc? -> (values undead-set-tree/rloc? undead-set/rloc?)
  (define/contract (undead-analysis-tail t uo)
    (-> any/c undead-set/rloc?
        (values undead-set-tree/rloc? undead-set/rloc?))

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
       (values (list utp ut1 ut2) uip)]
      [`(jump ,trg ,loc ...)
       (values
         loc
         (set-union (undead-analysis-trg trg uo) loc))]))

  ;; Computes the undead-in set for instruction e
  ;; given the undead-out set for e.
  ;;
  ;; effect undead-set/rloc? -> (values undead-set-tree/rloc? undead-set/rloc?)
  (define/contract (undead-analysis-effect e uo)
    (-> any/c undead-set/rloc?
        (values undead-set-tree/rloc? undead-set/rloc?))

    (match e
      [`(set! ,loc (,_binop ,loc ,opand))
        ;; 1. define loc -> set-remove
        ;; 2. reference loc -> set-add
        ;; 3. reference opand -> undead-analysis-opand
        (values
          uo
          (undead-analysis-opand
            opand
            (set-add uo loc)))]
      [`(set! ,loc ,triv)
        (values
          uo
          (undead-analysis-triv
            triv
            (set-remove uo loc)))]
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
  (define/contract (undead-analysis-pred p uo)
    (-> any/c undead-set/rloc? (values undead-set-tree/rloc? undead-set/rloc?))
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
      [`(if ,ppred ,pred1 ,pred2)
       (define-values (ut1 ui1) (undead-analysis-pred pred1 uo))
       (define-values (ut2 ui2) (undead-analysis-pred pred2 uo))
       (define-values (utp uip) (undead-analysis-pred ppred (set-union ui1 ui2)))
       (values (list utp ut1 ut2) uip)]
      [`(,relop ,loc ,opand)
       (values uo (undead-analysis-opand opand (set-add uo loc)))]))

  ;; Returns uo with o if o is a loc,
  ;; otherwise returns uo.
  ;;
  ;; o: opand
  ;; uo: undead-set/rloc?
  (define/contract (undead-analysis-opand o uo)
    (-> any/c undead-set/rloc? undead-set/rloc?)
    (match o
      [(? int64?) uo]
      [loc (set-add uo loc)]))

  ;; Returns uo with t if t is a loc,
  ;; otherwise returns uo.
  ;;
  ;; t: triv
  ;; uo: undead-set/rloc?
  (define/contract (undead-analysis-triv t uo)
    (-> any/c undead-set/rloc? undead-set/rloc?)
    (match t
      [(? label?) uo]
      [opand (undead-analysis-opand opand uo)]))

  ;; Returns uo with t if t is a loc,
  ;; otherwise returns uo.
  ;;
  ;; t: trg
  ;; uo: undead-set/rloc?
  (define/contract (undead-analysis-trg t uo)
    (-> any/c undead-set/rloc? undead-set/rloc?)
    (match t
      [(? label?) uo]
      [loc (set-add uo loc)]))

  ;; Don't need
  #;
  (define (undead-analysis-binop b uo)
    (match b
      ['* (void)]
      ['+ (void)]))
  (undead-analysis-p p))

(module+ test
  (require
    rackunit
    "../utils/test-utils.rkt")

  ;; For CPSC411 test suite
  (require
   rackunit/text-ui
   cpsc411/test-suite/public/v2-reg-alloc)

  (define/contract (set-equal? s1 s2)
    (-> list? list? boolean?)
    (empty? (set-symmetric-difference s1 s2)))

  (check-true (set-equal? '(1 2) '(1 2)))
  (check-true (set-equal? '(2 1) '(1 2)))
  (check-false (set-equal? '(2 1 3) '(1 2)))
  (check-false (set-equal? '(2 1) '(1 2 3)))

  ;; Quick helper to check that two undead-set-trees
  ;; are equal.
  (define/contract (ust-equal? have want)
    (-> undead-set-tree/rloc? undead-set-tree/rloc? boolean?)
    (match (cons have want)
      [(cons '() '()) #t]
      [(cons '() _) #f]
      [(cons _ '()) #f]
      [(cons s1 s2)
       #:when (not (list? (car s1)))
       (set-equal? s1 s2)]
      [(cons s1 s2)
       (and (equal? (length s1) (length s2))
            (andmap ust-equal? s1 s2))]))
  (check-true (ust-equal? '() '()))
  (check-true (ust-equal? '(x.1) '(x.1)))
  (check-true (ust-equal? '((x.1 x.2) (x.1)) '((x.1 x.2) (x.1))))
  (check-true (ust-equal? '((x.2 x.1) (x.1)) '((x.1 x.2) (x.1))))
  (check-true (ust-equal? '((((x.2 x.1) (x.1)))) '((((x.2 x.1) (x.1))))))

  ;; Check for ust-equal?
  ;; ust ust -> void
  (define-binary-check (check-ust-equal? have want)
    (ust-equal? have want))

  ;; Checks that the ust produced by `undead-analysis` is
  ;; equal to want. Also checks that the rest of the program
  ;; is the same.
  ;;
  ;; locals: (loc ...)
  ;; tail: tail
  ;; want: the expected locals
  (define-check (check-ust locals tail want)
    (match (undead-analysis `(module ([locals ,locals]) ,tail))
      [`(module ,info ,tail2)
        ;; Should just contain locals and undead-out
        (check-equal?
          (length info)
          2)
        (check-ust-equal?
          (info-ref info 'undead-out)
          want)
        ;; Should remain unchanged
        (check-equal? tail2 tail)]))

  ;; Check usts produced by 'undead-analysis' for tail and each of the procs
  ;; p info (List-of info) -> void
  (define-check (check-ust-proc program ust-tail ust-procs)
    ;; get fields from original program
    (define-values (main-info main-tail proc-labels proc-infos proc-tails)
      (extract-asm-pred-lang program))
    ;; get fields from compiled program
    (define-values (ua-main-info ua-main-tail ua-proc-labels ua-proc-infos ua-proc-tails)
      (extract-asm-pred-lang (undead-analysis program)))
    ;; keeps locals
    (check-equal? (info-ref ua-main-info 'locals) (info-ref main-info 'locals))
    (check-equal? ua-main-tail main-tail)
    (check-equal?
      (map (lambda (x) (info-ref x 'locals)) ua-proc-infos)
      (map (lambda (x) (info-ref x 'locals)) proc-infos))
    ;; tails is the same
    (check-equal? ua-proc-tails proc-tails)
    ;; check undead-out is as expected
    (check-ust-equal? (info-ref ua-main-info 'undead-out) ust-tail)
    (for ([ua-proc-info ua-proc-infos] [ust-proc ust-procs])
      (check-ust-equal? (info-ref ua-proc-info 'undead-out) ust-proc)))

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

  ;; rlocs
  (check-ust
    '(u.1)
    '(begin
      (set! r8 9)
      (set! rcx 9)
      (set! rdx 7)
      (set! u.1 99)
      (set! fv0 rcx)
      (set! fv1 u.1)
      (set! rdx (* rdx fv0))
      (if (< rcx fv1) (set! rcx 2) (set! fv2 9))
      (if (not (!= fv2 r8)) (halt rdx) (halt fv2)))
    '((fv2 r8)
      (fv2 r8 rcx)
      (rdx fv2 r8 rcx)
      (rdx fv2 r8 rcx u.1)
      (rdx fv2 r8 rcx fv0 u.1)
      (rdx fv2 r8 rcx fv1 fv0)
      (rdx fv2 r8 rcx fv1)
      ((rdx fv2 r8) (rdx fv2 r8) (rdx fv2 r8))
      ((rdx fv2) () ())))

  ;; proc & jumps base cases
  (check-ust-proc
    '(module ((locals ()))
      (jump done))
    '()
    (list))
  (check-ust-proc
    '(module ((locals ()))
      (define L.test.1 ((locals ())) (halt 0))
      (define L.test.2 ((locals ())) (halt 3))
      (jump L.test.1))
    '()
    (list '() '()))

  ;; simple jump
  (check-ust-proc
    '(module ((locals (x.1)))
      (begin
        (set! x.1 1)
        (set! rsi 9)
        (set! fv0 22)
        (set! fv1 done)
        (jump fv1 x.1 rsi fv0)))
    '((x.1)
      (x.1 rsi)
      (x.1 rsi fv0)
      (x.1 rsi fv0 fv1)
      (x.1 rsi fv0))
    (list))

  ;; complex jumps & procs
  (check-ust-proc
    '(module ((locals (u.1)))
      (define L.test.1 ((locals ()))
        (begin (set! r13 10) (set! r9 rdi) (halt r13)))
      (define L.test.2 ((locals (x.1)))
        (begin (set! x.1 r13) (set! rdx x.1) (set! fv0 9) (jump x.1 x.1 rdx fv0)))
      (begin
        (set! r8 9)
        (set! rcx 4)
        (set! fv0 rcx)
        (set! u.1 10)
        (set! r13 88)
        (if (>= r8 17)
            (begin
              (set! rdx rcx)
              (set! fv2 10)
              (jump L.test.1 rdx fv0 fv2))
            (begin
              (set! r14 L.test.2)
              (set! fv1 rcx)
              (set! fv2 fv1)
              (jump r14 r13 fv1 fv2 u.1)))))
    '((r8)
      (rcx r8)
      (rcx fv0 r8)
      (rcx fv0 u.1 r8)
      (rcx fv0 r13 u.1 r8)
      ((rcx fv0 r13 u.1)
       ((rdx fv0) (rdx fv0 fv2) (rdx fv0 fv2))
       ((r14 r13 u.1 rcx) (r14 r13 fv1 u.1) (r14 r13 fv1 fv2 u.1) (r13 fv1 fv2 u.1))))
    (list '((r13 rdi) (r13) ()) '((x.1) (x.1 rdx) (x.1 rdx fv0) (x.1 rdx fv0))))
  )

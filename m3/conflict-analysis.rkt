#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v5
  "../utils/compiler-utils.rkt")

(provide conflict-analysis)

;; Milestone 3 Exercise 2
;; Milestone 4 Exercise 14
;; Milestone 5 Exercise 9
;;
;; Create a conflict graph for the Asm-lang program with the undead-set tree
(define/contract (conflict-analysis p)
  (-> asm-pred-lang-v5/undead? asm-pred-lang-v5/conflicts?)

  (define conflict-graph (void))

  ;; Return true if l is loc, false otherwise
  (define (loc? l)
    (or (aloc? l) (register? l) (fvar? l)))
  
  ;; Performs conflict-analysis and adds conflicts to info of proc
  ;; proc ::= (define label info tail)
  ;;
  ;; label info tail -> proc
  (define (conflict-analysis-proc label info tail)
    (set! conflict-graph (new-graph (info-ref info 'locals)))
    (conflict-tail tail (info-ref info 'undead-out))
    (define newinfo (info-set info 'conflicts conflict-graph))
    `(define ,label ,newinfo ,tail))

  ;; Updates conflict graph for the locs in the tail
  ;; tail undead-set-tree/rloc -> void
  (define (conflict-tail tail ust)
    (match (cons tail ust)
      [(cons `(halt ,_) _) (void)]
      [(cons `(jump ,trg ,loc ...) _) (void)]
      [(cons `(begin ,effects ... ,tail) `(,usts ...))
       (for ([e effects] [u usts])
         (conflict-effect e u))
       (conflict-tail tail (last ust))]
      [(cons `(if ,pred ,tail1 ,tail2) `(,ustp ,ust1 ,ust2))
       (conflict-pred pred ustp)
       (conflict-tail tail1 ust1)
       (conflict-tail tail2 ust2)]))
  
  ;; Updates conflict graph for the locs in the effect
  ;; effect undead-set-tree/rloc -> void
  (define (conflict-effect effect ust)
    (match (cons effect ust)
      [(cons `(set! ,loc (,_ ,loc ,_)) undead-out)
       (for ([u undead-out])
         (unless
           (equal? loc u)
           (set! conflict-graph (add-edge conflict-graph loc u))))]
      [(cons `(set! ,loc ,triv) undead-out)
       (for ([u undead-out])
         (unless
           (or (equal? loc u) (and (loc? triv) (equal? triv u)))
           (set! conflict-graph (add-edge conflict-graph loc u))))]
      [(cons `(begin ,effects ...) `(,usts ...))
       (for ([e effects] [u usts])
         (conflict-effect e u))]
      [(cons `(if ,pred ,effect1 ,effect2) `(,ustp ,ust1 ,ust2))
       (conflict-pred pred ustp)
       (conflict-effect effect1 ust1)
       (conflict-effect effect2 ust2)]))

  ;; Updates conflict graph for the locs in the pred
  ;; pred undead-set-tree/rloc -> void
  (define (conflict-pred pred ust)
    (match (cons pred ust)
      [(cons `(true) _)
       (void)]
      [(cons `(false) _)
       (void)]
      [(cons `(not ,ipred) iust)
       (conflict-pred ipred iust)]
      [(cons `(begin ,effect ... ,tpred) `(,usts ...))
       (for ([e effect] [u usts])
         (conflict-effect e u))
       (conflict-pred tpred (last usts))]
      [(cons `(if ,ppred ,pred1 ,pred2) `(,ustp ,ust1 ,ust2))
       (conflict-pred ppred ustp)
       (conflict-pred pred1 ust1)
       (conflict-pred pred2 ust2)]
      [(cons `(,relop ,_ ,_) _)
       #:when(relop? relop)
       (void)]))

  (match p
    [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
     (set! conflict-graph (new-graph (info-ref info 'locals)))
     (conflict-tail tail (info-ref info 'undead-out))
     (define newinfo (info-set info 'conflicts conflict-graph))
     `(module ,newinfo ,@(map conflict-analysis-proc labels infos tails) ,tail)]))

;; graph graph -> boolean
;; Return true if graph associates each vertex to the same set of vertices
(define (graph-equals? g1 g2)
  (define keys1 (sort (sequence->list (in-dict-keys g1)) symbol<?))
  (define keys2 (sort (sequence->list (in-dict-keys g2)) symbol<?))
  (and (equal? (length g1) (length g2))
       (equal? keys1 keys2)
       (for/and ([k keys1])
         (define s1 (get-neighbors g1 k))
         (define s2 (get-neighbors g2 k))
         (and
           (equal? (length s1) (length s2))
           (equal? (list->set s1) (list->set s2))))))

(module+ test
  (require
    rackunit
    "../utils/test-utils.rkt")

  ;; graph-equals? tests
  (check-true (graph-equals? '() '()))
  (check-false (graph-equals? '() '((x.1 (x.2)))))
  (check-true (graph-equals? '((x.1 (x.2))) '((x.1 (x.2)))))
  ; check key and value flipped
  (check-false (graph-equals? '((x.1 (x.2))) '((x.2 (x.1)))))
  ; check duplicate in values
  (check-false (graph-equals? '((x.1 (x.2 x.2))) '((x.2 (x.1)))))
  ; check duplicate associations
  (check-false (graph-equals? '((x.1 (x.2)) (x.1 (x.2))) '((x.1 (x.2)))))
  ; check out of order
  (check-true (graph-equals?
    '((x.1 (x.2 x.3)) (x.2 (x.1 x.3)) (x.3 (x.4)))
    '((x.2 (x.3 x.1)) (x.3 (x.4)) (x.1 (x.3 x.2)))))
  ; check extra vertex
  (check-false (graph-equals?
    '((x.1 (x.2 x.3)) (x.2 (x.1 x.3)) (x.3 (x.4)))
    '((x.1 (x.2 x.3)) (x.2 (x.1 x.3)) (x.3 (x.4)) (x.4 ()))))
  ; check extra edge
  (check-false (graph-equals?
    '((x.1 (x.2 x.3)) (x.2 (x.1 x.3)) (x.3 (x.4 x.2)) (x.4 (x.3)))
    '((x.1 (x.2 x.3)) (x.2 (x.1 x.3)) (x.3 (x.4)) (x.4 (x.3)))))

  ;; Check that the graphs are equal
  ;; any any -> void
  (define-binary-check (check-graph? actual expected)
    (graph-equals? actual expected))

  ;; Check that a program and undead-out compiles into a program w/ conflicts
  ;; by compiling with conflict-analysis
  ;; asm-pred-lang-v5/locals undead-set-tree/rloc conflicts -> void
  (define-check (check-conflict program undead-out conflicts-tail)
    (match program
      [`(module ,info ,procs ... ,tail)
        (check-conflict-proc
          `(module ,(info-set info 'undead-out undead-out) ,@procs ,tail)
          conflicts-tail
          '())]))
  
  ;; Check that a program w/ procs & undead-out compiles into
  ;; a program w/ procs & conflicts by compiling with conflict-analysis
  ;; asm-pred-lang-v5/undead conflicts (List-of conflicts) -> void
  (define-check (check-conflict-proc program conflicts-tail conflicts-procs)
    ;; get fields from original program
    (define-values (main-info main-tail proc-labels proc-infos proc-tails)
      (extract-asm-pred-lang program))
    ;; get fields from compiled program
    (define-values (ca-main-info ca-main-tail ca-proc-labels ca-proc-infos ca-proc-tails)
      (extract-asm-pred-lang (conflict-analysis program)))
    ;; keeps locals
    (check-equal? (info-ref ca-main-info 'locals) (info-ref main-info 'locals))
    (check-equal? ca-main-tail main-tail)
    (check-equal?
      (map (lambda (x) (info-ref x 'locals)) ca-proc-infos)
      (map (lambda (x) (info-ref x 'locals)) proc-infos))
    ;; tails is the same
    (check-equal? ca-proc-tails proc-tails)
    ;; check conflicts is as expected
    (check-graph? (info-ref ca-main-info 'conflicts) conflicts-tail)
    (for ([ca-proc-info ca-proc-infos] [conflict-proc conflicts-procs])
      (check-graph? (info-ref ca-proc-info 'conflicts) conflict-proc)))
  
  ;; conflict-analysis tests
  (check-conflict
    '(module
        ((locals ()))
        (begin (halt 0)))
    '(())
    '())

  ; No conflicts
  (check-conflict
    '(module
        ((locals (x.1 x.2)))
        (begin (set! x.2 12) (set! x.1 x.2) (halt x.1)))
    '((x.2) (x.1) ())
    '((x.1 ()) (x.2 ())))

  ; One-to-one conflict
  (check-conflict
    '(module
        ((locals (x.1 x.2)))
        (begin
          (set! x.1 5)
          (begin
            (set! x.2 10)
            (set! x.1 (+ x.1 x.2)))
          (halt 0)))
    '((x.1)
      ((x.1 x.2)
       ())
      ())
    '((x.1 (x.2)) (x.2 (x.1))))

  ; Move optimization
  (check-conflict
    '(module
        ((locals (x.1 x.2 x.3)))
        (begin
          (set! x.1 5)
          (set! x.2 x.1)
          (set! x.3 x.1)
          (halt x.2)))
    '((x.1)
      (x.1 x.2)
      (x.2)
      ())
    '((x.1 ()) (x.2 (x.3)) (x.3 (x.2))))
  
  ; No move optimization for binop
  (check-conflict
    '(module
        ((locals (x.1 x.2 x.3)))
        (begin
          (set! x.1 5)
          (set! x.2 x.1)
          (set! x.2 (+ x.2 x.1))
          (set! x.3 x.1)
          (halt x.2)))
    '((x.1)
      (x.1 x.2)
      (x.1 x.2)
      (x.2)
      ())
    '((x.1 (x.2)) (x.2 (x.1 x.3)) (x.3 (x.2))))

  ; Many alocs + conflicts
  (check-conflict
    '(module
      ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1 a.10)))
      (begin
        (set! v.1 1)
        (set! w.2 46)
        (begin (set! x.3 v.1))
        (set! p.1 7)
        (set! x.3 (+ x.3 p.1))
        (set! y.4 x.3)
        (begin
          (set! p.1 4)
          (begin
            (set! y.4 (+ y.4 p.1))
            (set! z.5 x.3)
            (set! z.5 (+ z.5 w.2))))
        (set! t.6 y.4)
        (set! p.1 -1)
        (set! a.10 10)
        (set! a.10 (* a.10 10))
        (begin
          (set! t.6 (* t.6 p.1))
          (set! z.5 (+ z.5 t.6))
          (begin (halt z.5)))))
    '((v.1)
      (v.1 w.2)
      ((w.2 x.3))
      (p.1 w.2 x.3)
      (w.2 x.3)
      (y.4 w.2 x.3)
      ((p.1 y.4 w.2 x.3)
       ((y.4 w.2 x.3)
        (z.5 y.4 w.2)
        (z.5 y.4)))
      (t.6 z.5)
      (t.6 z.5 p.1)
      (t.6 z.5 p.1 a.10)
      (t.6 z.5 p.1)
      ((t.6 z.5)
       (z.5)
       (())))
    '((v.1 (w.2))
      (w.2 (z.5 y.4 p.1 x.3 v.1))
      (x.3 (y.4 p.1 w.2))
      (y.4 (z.5 x.3 p.1 w.2))
      (z.5 (p.1 t.6 w.2 y.4 a.10))
      (t.6 (p.1 z.5 a.10))
      (p.1 (z.5 t.6 y.4 x.3 w.2 a.10))
      (a.10 (t.6 z.5 p.1))))

  ; if in tail
  (check-conflict
    '(module
      ((locals (x.1 x.2 x.3)))
      (begin
        (set! x.1 5)
        (if (= x.1 2)
          (begin (set! x.2 10) (set! x.1 (+ x.1 x.2)) (halt x.1))
          (begin (set! x.3 9) (set! x.1 x.3) (halt x.1)))))
    '((x.1)
      ((x.1)
       ((x.1 x.2) (x.1) ())
       ((x.3) (x.1) ())))
    '((x.1 (x.2)) (x.2 (x.1)) (x.3 ())))
  
   ; if in effect
  (check-conflict
    '(module
      ((locals (x.1 x.2 x.3)))
      (begin
        (set! x.1 5)
        (if (not (>= x.1 2))
          (begin (set! x.3 9) (set! x.1 x.3))
          (begin (set! x.2 10) (set! x.1 (* x.1 x.2))))
        (halt x.1)))
    '((x.1)
      ((x.1)
       ((x.3) (x.1))
       ((x.1 x.2) (x.1)))
      ())
    '((x.1 (x.2)) (x.2 (x.1)) (x.3 ())))
  
  ;; if in pred
  (check-conflict
    '(module
      ((locals (x.1 x.2 x.3 x.4 x.5 x.6 x.7)))
      (begin
        (set! x.6 0)
        (set! x.1 5)
        (set! x.2 8)
        (set! x.4 x.2)
        (if
          (not
            (begin
              (set! x.4 (+ x.4 9))
              (if (= x.2 x.4)
                  (begin
                    (set! x.3 9)
                    (set! x.1 x.3)
                    (set! x.5 9)
                    (= x.1 x.5))
                  (begin
                    (set! x.3 10)
                    (set! x.4 (* x.4 9))
                    (set! x.7 x.3)
                    (set! x.1 (* x.1 x.3))
                    (not (< x.4 x.7))))))
          (halt x.2)
          (halt x.3))))
    '(()
      (x.1)
      (x.1 x.2)
      (x.4 x.1 x.2)
      (((x.4 x.1 x.2)
        ((x.4 x.1 x.2)
         ((x.3 x.2)
          (x.1 x.3 x.2)
          (x.5 x.1 x.3 x.2)
          (x.3 x.2))
         ((x.1 x.4 x.3 x.2)
          (x.1 x.4 x.3 x.2)
          (x.1 x.7 x.4 x.3 x.2)
          (x.7 x.4 x.3 x.2)
          (x.3 x.2))))
       ()
       ()))
    '((x.1 (x.2 x.3 x.4 x.5 x.7))
      (x.2 (x.1 x.3 x.4 x.5 x.7))
      (x.3 (x.1 x.2 x.4 x.5))
      (x.4 (x.1 x.2 x.3 x.7))
      (x.5 (x.1 x.2 x.3))
      (x.6 ())
      (x.7 (x.1 x.2 x.4))))

  ;; move optimization for rloc
  (check-conflict
    '(module
        ((locals ()))
        (begin
          (set! rbx 5)
          (set! rdx rbx)
          (set! rsi rbx)
          (set! fv0 2)
          (set! fv1 fv0)
          (set! fv2 fv0)
          (halt rdx)))
    '((rbx)
      (rbx rdx)
      (rdx)
      (rdx fv0)
      (rdx fv0)
      (rdx)
      ())
    '((rsi (rdx)) (rdx (fv2 fv1 fv0 rsi)) (fv0 (rdx)) (fv1 (rdx)) (fv2 (rdx))))
  
  ;; proc & jumps base cases
  (check-conflict
    '(module ((locals ()))
      (jump done))
    '()
    (new-graph))
  (check-conflict-proc
    '(module ((locals ()) (undead-out ()))
      (define L.test.1 ((locals ()) (undead-out ())) (halt 0))
      (define L.test.2 ((locals ()) (undead-out ())) (halt 3))
      (jump L.test.1))
    '()
    (list (new-graph) (new-graph)))

  ;; complex jumps & procs
  (check-conflict-proc
    '(module
      ((locals (u.1))
       (undead-out
         ((r8)
          (r8 rcx)
          (r8 rcx fv0)
          (r8 u.1 rcx fv0)
          (r8 r13 u.1 rcx fv0)
          ((r13 u.1 rcx fv0)
           ((fv0 rdx) (fv2 fv0 rdx) (fv2 fv0 rdx))
           ((rcx r14 u.1 r13)
            (r14 u.1 fv1 r13)
            (r14 u.1 fv2 fv1 r13)
            (u.1 fv2 fv1 r13))))))
      (define L.test.1
        ((locals ())
         (undead-out ((rdi r13) (r13) ())))
        (begin (set! r13 10) (set! r9 rdi) (halt r13)))
      (define L.test.2
        ((locals (x.1))
         (undead-out ((x.1) (rdx x.1) (fv0 rdx x.1) (fv0 rdx x.1))))
        (begin (set! x.1 r13) (set! rdx x.1) (set! fv0 9) (jump x.1 x.1 rdx fv0)))
      (begin
        (set! r8 9)
        (set! rcx 4)
        (set! fv0 rcx)
        (set! u.1 10)
        (set! r13 88)
        (if (>= r8 17)
          (begin (set! rdx rcx) (set! fv2 10) (jump L.test.1 rdx fv0 fv2))
          (begin
            (set! r14 L.test.2)
            (set! fv1 rcx)
            (set! fv2 fv1)
            (jump r14 r13 fv1 fv2 u.1)))))
    '((u.1 (r13 fv0 rcx r8 fv2 fv1 r14))
      (r14 (fv2 fv1 r13 u.1 rcx))
      (rcx (r13 u.1 r8 r14))
      (r13 (fv0 rcx u.1 r8 fv2 fv1 r14))
      (fv1 (r13 u.1 r14))
      (fv2 (rdx fv0 r13 u.1 r14))
      (rdx (fv2 fv0))
      (fv0 (r13 u.1 r8 fv2 rdx))
      (r8 (r13 u.1 fv0 rcx)))
    (list
      '((r13 (r9 rdi))
        (rdi (r13))
        (r9 (r13)))
      '((x.1 (fv0))
        (fv0 (x.1 rdx))
        (rdx (fv0)))))
  )
#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7
  cpsc411/graph-lib)

(provide assign-call-undead-variables)

;; Milestone 6 Exercise 10
;; Milestone 7 Exercise 7
;;
;; Compiles Asm-pred-lang-v7/conflicts to Asm-pred-lang-v7/pre-framed by
;; pre-assigning all variables in the call-undead sets to frame variables.
;; Only info is modified.
(define/contract (assign-call-undead-variables p)
  (-> asm-pred-lang-v7/conflicts? asm-pred-lang-v7/pre-framed?)

  ;; Return a set of frame variables that are incompatible with x.
  ;;
  ;; A variable x is compatible with a frame location fvar_i if it is not
  ;; directly in conflict with fvar_i, and it is not in conflict with a
  ;; variable y that has been assigned to fvar_i.
  ;;
  ;; x: loc
  ;; cg: conflict-graph
  (define/contract (incompatible-fvars x cg as)
    (-> symbol? graph? (listof (list/c aloc? fvar?)) (listof fvar?))
    (define x-conflicts (get-neighbors cg x))
    (set-union
      ;; fvars that directly conflict with x
      (filter fvar? x-conflicts)
      (for/fold ([acc '()])
                ([a as])
        (if (set-member? x-conflicts (first a))
          (cons (second a) acc)
          acc))))

  ;; Return the first fvar that is not in incompatible.
  (define/contract (assign-first-fvar incompatible)
    (-> (listof fvar?) fvar?)
    (let f ([i 0])
      (define fvar (make-fvar i))
      (if (set-member? incompatible fvar)
        (f (add1 i))
        fvar)))

  ;; Assign all call undead variables using the info in info.
  (define/contract (info->assignment info)
    (-> info? (listof (list/c aloc? fvar?)))
    ;; cu: call undead set
    ;; cg: conflict graph
    ;; -> (list/c aloc? fvar?)
    (let f ([cu (info-ref info 'call-undead)]
            [cg (info-ref info 'conflicts)])
      (if (empty? cu)
        '()
        (let ([as (f (cdr cu) (remove-vertex cg (car cu)))])
          (cons
            `(,(car cu)
               ,(assign-first-fvar
                  (incompatible-fvars (car cu) cg as)))
            as)))))

  ;; Removes 'undead-out and adds assignments.
  ;;
  ;; asm-pred-lang-v7/conflicts-info -> asm-pred-lang-v7/pre-framed-info
  (define/contract (update-info info)
    (-> info? info?)
    (define assignments (info->assignment info))
    (info-set
      (info-set
        info
        'locals
        (foldl
          (lambda (a locals) (set-remove locals (first a)))
          (info-ref info 'locals)
          assignments))
      'assignment
      assignments))


  ;; Assigns call undead variables for a procedure with the given label,
  ;; info, and tail. Only the info field is modified in the returned procedure.
  ;;
  ;; -> asm-pred-lang-v7/pre-framed-procedure
  (define/contract (assign-call-undead-variables-proc label info tail)
    (-> label? info? any/c any/c)
    `(define
       ,label
       ,(update-info info)
       ,tail))

  (define (assign-call-undead-variables-p p)
    (match p
      [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
        `(module
           ,(update-info info)
           ,@(map assign-call-undead-variables-proc labels infos tails)
           ,tail)]))

  ;; Modified template - deleted all unused functions.

  (assign-call-undead-variables-p p))

(module+ test
  (require rackunit)

  ;; values-lang-v7: '(module 42)
  (check-match
    (assign-call-undead-variables
      '(module
         ((new-frames ())
          (locals (tmp-ra.1))
          (call-undead ())
          (undead-out ((tmp-ra.1 rbp) (tmp-ra.1 rax rbp) (rax rbp)))
          (conflicts
           ((tmp-ra.1 (rax rbp)) (rbp (rax tmp-ra.1)) (rax (rbp tmp-ra.1)))))
         (begin (set! tmp-ra.1 r15) (set! rax 42) (jump tmp-ra.1 rbp rax))))
    `(module
       ,info
       (begin (set! tmp-ra.1 r15) (set! rax 42) (jump tmp-ra.1 rbp rax)))
    (and
      (equal? '() (info-ref info 'assignment))
      ))

  ;; book example 1
  (check-match
    (assign-call-undead-variables
      '(module
         ((new-frames ())
          (locals (tmp-ra.4))
          (call-undead ())
          (undead-out
           ((tmp-ra.4 rbp)
            (tmp-ra.4 fv1 rbp)
            (tmp-ra.4 fv1 fv0 rbp)
            (fv1 fv0 r15 rbp)
            (fv1 fv0 r15 rbp)))
          (conflicts
           ((tmp-ra.4 (fv0 fv1 rbp))
            (rbp (r15 fv0 fv1 tmp-ra.4))
            (fv1 (r15 fv0 rbp tmp-ra.4))
            (fv0 (r15 rbp fv1 tmp-ra.4))
            (r15 (rbp fv0 fv1)))))
         (define L.swap.1
           ((new-frames ((nfv.2 nfv.3)))
            (locals (nfv.2 nfv.3 z.3 tmp-ra.1 x.1 y.2))
            (undead-out
             ((fv0 fv1 tmp-ra.1 rbp)
              (fv1 x.1 tmp-ra.1 rbp)
              (y.2 x.1 tmp-ra.1 rbp)
              ((y.2 x.1 tmp-ra.1 rbp)
               ((tmp-ra.1 rax rbp) (rax rbp))
               (((rax tmp-ra.1 rbp)
                 ((y.2 nfv.3 rbp)
                  (nfv.3 nfv.2 rbp)
                  (nfv.3 nfv.2 r15 rbp)
                  (nfv.3 nfv.2 r15 rbp)))
                (z.3 tmp-ra.1 rbp)
                (tmp-ra.1 rax rbp)
                (rax rbp)))))
            (call-undead (tmp-ra.1))
            (conflicts
             ((y.2 (rbp tmp-ra.1 x.1 nfv.3))
              (x.1 (y.2 rbp tmp-ra.1 fv1))
              (tmp-ra.1 (y.2 x.1 rbp fv1 fv0 rax z.3))
              (z.3 (rbp tmp-ra.1))
              (nfv.3 (r15 nfv.2 rbp y.2))
              (nfv.2 (r15 rbp nfv.3))
              (rbp (y.2 x.1 tmp-ra.1 rax z.3 r15 nfv.2 nfv.3))
              (r15 (rbp nfv.2 nfv.3))
              (rax (rbp tmp-ra.1))
              (fv0 (tmp-ra.1))
              (fv1 (x.1 tmp-ra.1)))))
           (begin
             (set! tmp-ra.1 r15)
             (set! x.1 fv0)
             (set! y.2 fv1)
             (if (< y.2 x.1)
               (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
               (begin
                 (return-point L.rp.1
                   (begin
                     (set! nfv.3 x.1)
                     (set! nfv.2 y.2)
                     (set! r15 L.rp.1)
                     (jump L.swap.1 rbp r15 nfv.2 nfv.3)))
                 (set! z.3 rax)
                 (set! rax z.3)
                 (jump tmp-ra.1 rbp rax)))))
         (begin
           (set! tmp-ra.4 r15)
           (set! fv1 2)
           (set! fv0 1)
           (set! r15 tmp-ra.4)
           (jump L.swap.1 rbp r15 fv0 fv1))))
    `(module
       ,info
       (define L.swap.1
         ,info-swap
         (begin
           (set! tmp-ra.1 r15)
           (set! x.1 fv0)
           (set! y.2 fv1)
           (if (< y.2 x.1)
             (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
             (begin
               (return-point L.rp.1
                 (begin
                   (set! nfv.3 x.1)
                   (set! nfv.2 y.2)
                   (set! r15 L.rp.1)
                   (jump L.swap.1 rbp r15 nfv.2 nfv.3)))
               (set! z.3 rax)
               (set! rax z.3)
               (jump tmp-ra.1 rbp rax)))))
       (begin
         (set! tmp-ra.4 r15)
         (set! fv1 2)
         (set! fv0 1)
         (set! r15 tmp-ra.4)
         (jump L.swap.1 rbp r15 fv0 fv1)))
    (and
      (empty? (info-ref info 'assignment))
      (equal? '(tmp-ra.4) (info-ref info 'locals))


      (equal? '((tmp-ra.1 fv2)) (info-ref info-swap 'assignment))
      ;; locals should be updated
      (equal? (list->set '(y.2 x.1 z.3 nfv.3 nfv.2)) (list->set (info-ref info-swap 'locals)))
      ;; new-frames, locals, undead-out, call-undead, conflicts, assignment
      (= 6 (length info-swap))
      ;; the rest should remain unchanged
      (equal? '((nfv.2 nfv.3)) (info-ref info-swap 'new-frames))
      (equal? '(tmp-ra.1) (info-ref info-swap 'call-undead))
      (equal?
        '((y.2 (rbp tmp-ra.1 x.1 nfv.3))
          (x.1 (y.2 rbp tmp-ra.1 fv1))
          (tmp-ra.1 (y.2 x.1 rbp fv1 fv0 rax z.3))
          (z.3 (rbp tmp-ra.1))
          (nfv.3 (r15 nfv.2 rbp y.2))
          (nfv.2 (r15 rbp nfv.3))
          (rbp (y.2 x.1 tmp-ra.1 rax z.3 r15 nfv.2 nfv.3))
          (r15 (rbp nfv.2 nfv.3))
          (rax (rbp tmp-ra.1))
          (fv0 (tmp-ra.1))
          (fv1 (x.1 tmp-ra.1)))
        (info-ref info-swap 'conflicts))))

  #;
  (parameterize ([current-parameter-registers '()])
    (pretty-display
     ((compose
       conflict-analysis
       undead-analysis
       uncover-locals
       select-instructions
       impose-calling-conventions
       normalize-bind
       sequentialize-let)
      '(module
         (define L.swap.1
           (lambda (x.1 y.2)
             (if (< y.2 x.1)
                 x.1
                 (let ([z.3 (call L.swap.1 y.2 x.1)]
                       [y.3 (call L.swap.1 y.2 x.1)])
                   z.3))))
         (call L.swap.1 1 2)))))
  (check-match
    (assign-call-undead-variables
      '(module
         ((new-frames ())
          (locals (tmp-ra.6))
          (call-undead ())
          (undead-out
           ((tmp-ra.6 rbp)
            (tmp-ra.6 fv1 rbp)
            (tmp-ra.6 fv1 fv0 rbp)
            (fv1 fv0 r15 rbp)
            (fv1 fv0 r15 rbp)))
          (conflicts
           ((tmp-ra.6 (fv0 fv1 rbp))
            (rbp (r15 fv0 fv1 tmp-ra.6))
            (fv1 (r15 fv0 rbp tmp-ra.6))
            (fv0 (r15 rbp fv1 tmp-ra.6))
            (r15 (rbp fv0 fv1)))))
         (define L.swap.1
           ((new-frames ((nfv.4 nfv.5) (nfv.2 nfv.3)))
            (locals (nfv.2 nfv.3 z.3 nfv.4 nfv.5 y.3 tmp-ra.1 x.1 y.2))
            (undead-out
             ((fv0 fv1 tmp-ra.1 rbp)
              (fv1 x.1 tmp-ra.1 rbp)
              (y.2 x.1 tmp-ra.1 rbp)
              ((y.2 x.1 tmp-ra.1 rbp)
               ((tmp-ra.1 rax rbp) (rax rbp))
               (((rax y.2 x.1 tmp-ra.1 rbp)
                 ((y.2 nfv.3 rbp)
                  (nfv.3 nfv.2 rbp)
                  (nfv.3 nfv.2 r15 rbp)
                  (nfv.3 nfv.2 r15 rbp)))
                (y.2 x.1 z.3 tmp-ra.1 rbp)
                ((rax z.3 tmp-ra.1 rbp)
                 ((y.2 nfv.5 rbp)
                  (nfv.5 nfv.4 rbp)
                  (nfv.5 nfv.4 r15 rbp)
                  (nfv.5 nfv.4 r15 rbp)))
                (z.3 tmp-ra.1 rbp)
                (tmp-ra.1 rax rbp)
                (rax rbp)))))
            (call-undead (z.3 y.2 x.1 tmp-ra.1))
            (conflicts
             ((y.2 (rbp tmp-ra.1 x.1 nfv.5 z.3 nfv.3))
              (x.1 (y.2 rbp tmp-ra.1 fv1 z.3))
              (tmp-ra.1 (y.2 x.1 rbp fv1 fv0 rax y.3 z.3))
              (y.3 (rbp tmp-ra.1 z.3))
              (nfv.5 (r15 nfv.4 rbp y.2))
              (nfv.4 (r15 rbp nfv.5))
              (z.3 (y.3 rbp tmp-ra.1 x.1 y.2))
              (nfv.3 (r15 nfv.2 rbp y.2))
              (nfv.2 (r15 rbp nfv.3))
              (rbp (y.2 x.1 tmp-ra.1 rax y.3 nfv.4 nfv.5 z.3 r15 nfv.2 nfv.3))
              (r15 (nfv.4 nfv.5 rbp nfv.2 nfv.3))
              (rax (rbp tmp-ra.1))
              (fv0 (tmp-ra.1))
              (fv1 (x.1 tmp-ra.1)))))
           (begin
             (set! tmp-ra.1 r15)
             (set! x.1 fv0)
             (set! y.2 fv1)
             (if (< y.2 x.1)
               (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
               (begin
                 (return-point L.rp.1
                   (begin
                     (set! nfv.3 x.1)
                     (set! nfv.2 y.2)
                     (set! r15 L.rp.1)
                     (jump L.swap.1 rbp r15 nfv.2 nfv.3)))
                 (set! z.3 rax)
                 (return-point L.rp.2
                   (begin
                     (set! nfv.5 x.1)
                     (set! nfv.4 y.2)
                     (set! r15 L.rp.2)
                     (jump L.swap.1 rbp r15 nfv.4 nfv.5)))
                 (set! y.3 rax)
                 (set! rax z.3)
                 (jump tmp-ra.1 rbp rax)))))
         (begin
           (set! tmp-ra.6 r15)
           (set! fv1 2)
           (set! fv0 1)
           (set! r15 tmp-ra.6)
           (jump L.swap.1 rbp r15 fv0 fv1))))
    `(module
       ,info
       (define L.swap.1
         ,info-swap
         (begin
           (set! tmp-ra.1 r15)
           (set! x.1 fv0)
           (set! y.2 fv1)
           (if (< y.2 x.1)
             (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
             (begin
               (return-point L.rp.1
                 (begin
                   (set! nfv.3 x.1)
                   (set! nfv.2 y.2)
                   (set! r15 L.rp.1)
                   (jump L.swap.1 rbp r15 nfv.2 nfv.3)))
               (set! z.3 rax)
               (return-point L.rp.2
                 (begin
                   (set! nfv.5 x.1)
                   (set! nfv.4 y.2)
                   (set! r15 L.rp.2)
                   (jump L.swap.1 rbp r15 nfv.4 nfv.5)))
               (set! y.3 rax)
               (set! rax z.3)
               (jump tmp-ra.1 rbp rax)))))
       (begin
         (set! tmp-ra.6 r15)
         (set! fv1 2)
         (set! fv0 1)
         (set! r15 tmp-ra.6)
         (jump L.swap.1 rbp r15 fv0 fv1)))
    (and
      (empty? (info-ref info 'assignment))
      (equal? '(tmp-ra.6) (info-ref info 'locals))

      (equal?
        ;; this might be a bit fragile...
        (list->set '((tmp-ra.1 fv2) (x.1 fv0) (y.2 fv1) (z.3 fv3)))
        (list->set (info-ref info-swap 'assignment)))
      ;; locals should be updated
      (equal? (list->set '(y.3 nfv.5 nfv.4 nfv.3 nfv.2)) (list->set (info-ref info-swap 'locals)))))

  #;
  (parameterize ([current-parameter-registers '()])
    (pretty-display
     ((compose
       conflict-analysis
       undead-analysis
       uncover-locals
       select-instructions
       impose-calling-conventions
       normalize-bind
       sequentialize-let
       uniquify)
      '(module
         (define foo (lambda () 10))
         (define bar (lambda () 11))
         (let ([a 1]
               [b 2]
               [c 3]
               [d 4]
               [e 5]
               [f 6])
           (let ([foo1 (call foo)])
             (let ([x (+ a b)]
                   [y (+ c d)]
                   [z (+ e f)])
               (let ([bar1 (call bar)]
                     [x (+ x foo1)])
                 (let ([tmp1 (+ x y)]
                       [tmp2 (+ z bar1)])
                   (+ tmp1 tmp2))))))))))
  (check-match
    (assign-call-undead-variables
      '(module
         ((new-frames (() ()))
          (locals
           (tmp2.13
            tmp1.14
            x.11
            bar1.12
            z.8
            y.9
            x.10
            foo1.7
            f.1
            e.2
            d.3
            c.4
            b.5
            a.6
            tmp-ra.17))
          (call-undead (z.8 y.9 foo1.7 x.10 a.6 b.5 c.4 d.3 e.2 f.1 tmp-ra.17))
          (undead-out
           ((tmp-ra.17 rbp)
            (a.6 tmp-ra.17 rbp)
            (a.6 b.5 tmp-ra.17 rbp)
            (a.6 b.5 c.4 tmp-ra.17 rbp)
            (a.6 b.5 c.4 d.3 tmp-ra.17 rbp)
            (a.6 b.5 c.4 d.3 e.2 tmp-ra.17 rbp)
            (a.6 b.5 c.4 d.3 e.2 f.1 tmp-ra.17 rbp)
            ((rax a.6 b.5 c.4 d.3 e.2 f.1 tmp-ra.17 rbp) ((r15 rbp) (r15 rbp)))
            (a.6 b.5 c.4 d.3 e.2 f.1 foo1.7 tmp-ra.17 rbp)
            (b.5 x.10 c.4 d.3 e.2 f.1 foo1.7 tmp-ra.17 rbp)
            (c.4 d.3 e.2 f.1 x.10 foo1.7 tmp-ra.17 rbp)
            (d.3 y.9 e.2 f.1 x.10 foo1.7 tmp-ra.17 rbp)
            (e.2 f.1 x.10 foo1.7 y.9 tmp-ra.17 rbp)
            (f.1 z.8 x.10 foo1.7 y.9 tmp-ra.17 rbp)
            (x.10 foo1.7 y.9 z.8 tmp-ra.17 rbp)
            ((rax x.10 foo1.7 y.9 z.8 tmp-ra.17 rbp) ((r15 rbp) (r15 rbp)))
            (x.10 foo1.7 y.9 z.8 bar1.12 tmp-ra.17 rbp)
            (foo1.7 x.11 y.9 z.8 bar1.12 tmp-ra.17 rbp)
            (x.11 y.9 z.8 bar1.12 tmp-ra.17 rbp)
            (y.9 tmp1.14 z.8 bar1.12 tmp-ra.17 rbp)
            (z.8 bar1.12 tmp1.14 tmp-ra.17 rbp)
            (bar1.12 tmp2.13 tmp1.14 tmp-ra.17 rbp)
            (tmp1.14 tmp2.13 tmp-ra.17 rbp)
            (tmp2.13 rax tmp-ra.17 rbp)
            (tmp-ra.17 rax rbp)
            (rax rbp)))
          (conflicts
           ((tmp-ra.17
             (rax
              tmp2.13
              tmp1.14
              x.11
              bar1.12
              z.8
              y.9
              x.10
              foo1.7
              f.1
              e.2
              d.3
              c.4
              b.5
              a.6
              rbp))
            (a.6 (foo1.7 f.1 e.2 d.3 c.4 b.5 rbp tmp-ra.17))
            (b.5 (x.10 foo1.7 f.1 e.2 d.3 c.4 rbp tmp-ra.17 a.6))
            (c.4 (x.10 foo1.7 f.1 e.2 d.3 rbp tmp-ra.17 b.5 a.6))
            (d.3 (y.9 x.10 foo1.7 f.1 e.2 rbp tmp-ra.17 c.4 b.5 a.6))
            (e.2 (y.9 x.10 foo1.7 f.1 rbp tmp-ra.17 d.3 c.4 b.5 a.6))
            (f.1 (z.8 y.9 x.10 foo1.7 rbp tmp-ra.17 e.2 d.3 c.4 b.5 a.6))
            (foo1.7 (x.11 bar1.12 z.8 y.9 x.10 rbp tmp-ra.17 f.1 e.2 d.3 c.4 b.5 a.6))
            (x.10 (bar1.12 z.8 y.9 rbp tmp-ra.17 foo1.7 f.1 e.2 d.3 c.4 b.5))
            (y.9 (tmp1.14 x.11 bar1.12 z.8 rbp tmp-ra.17 foo1.7 x.10 f.1 e.2 d.3))
            (z.8 (tmp1.14 x.11 bar1.12 rbp tmp-ra.17 y.9 foo1.7 x.10 f.1))
            (bar1.12 (tmp2.13 tmp1.14 x.11 rbp tmp-ra.17 z.8 y.9 foo1.7 x.10))
            (x.11 (rbp tmp-ra.17 bar1.12 z.8 y.9 foo1.7))
            (tmp1.14 (tmp2.13 rbp tmp-ra.17 bar1.12 z.8 y.9))
            (tmp2.13 (rax rbp tmp-ra.17 tmp1.14 bar1.12))
            (rbp
             (rax
              tmp2.13
              tmp1.14
              x.11
              bar1.12
              z.8
              y.9
              x.10
              foo1.7
              r15
              f.1
              e.2
              d.3
              c.4
              b.5
              a.6
              tmp-ra.17))
            (r15 (rbp))
            (rax (rbp tmp-ra.17 tmp2.13)))))
         (define L.foo.1
           ((new-frames ())
            (locals (tmp-ra.15))
            (undead-out ((tmp-ra.15 rbp) (tmp-ra.15 rax rbp) (rax rbp)))
            (call-undead ())
            (conflicts
             ((tmp-ra.15 (rax rbp)) (rbp (rax tmp-ra.15)) (rax (rbp tmp-ra.15)))))
           (begin (set! tmp-ra.15 r15) (set! rax 10) (jump tmp-ra.15 rbp rax)))
         (define L.bar.2
           ((new-frames ())
            (locals (tmp-ra.16))
            (undead-out ((tmp-ra.16 rbp) (tmp-ra.16 rax rbp) (rax rbp)))
            (call-undead ())
            (conflicts
             ((tmp-ra.16 (rax rbp)) (rbp (rax tmp-ra.16)) (rax (rbp tmp-ra.16)))))
           (begin (set! tmp-ra.16 r15) (set! rax 11) (jump tmp-ra.16 rbp rax)))
         (begin
           (set! tmp-ra.17 r15)
           (set! a.6 1)
           (set! b.5 2)
           (set! c.4 3)
           (set! d.3 4)
           (set! e.2 5)
           (set! f.1 6)
           (return-point L.rp.3 (begin (set! r15 L.rp.3) (jump L.foo.1 rbp r15)))
           (set! foo1.7 rax)
           (set! x.10 a.6)
           (set! x.10 (+ x.10 b.5))
           (set! y.9 c.4)
           (set! y.9 (+ y.9 d.3))
           (set! z.8 e.2)
           (set! z.8 (+ z.8 f.1))
           (return-point L.rp.4 (begin (set! r15 L.rp.4) (jump L.bar.2 rbp r15)))
           (set! bar1.12 rax)
           (set! x.11 x.10)
           (set! x.11 (+ x.11 foo1.7))
           (set! tmp1.14 x.11)
           (set! tmp1.14 (+ tmp1.14 y.9))
           (set! tmp2.13 z.8)
           (set! tmp2.13 (+ tmp2.13 bar1.12))
           (set! rax tmp1.14)
           (set! rax (+ rax tmp2.13))
           (jump tmp-ra.17 rbp rax))))
  `(module
     ,info
     (define L.foo.1 ,info-foo ,_)
     (define L.bar.2 ,info-bar ,_)
     ,_)
  (and
      (empty? (info-ref info-foo 'assignment))
      (equal? '(tmp-ra.15) (info-ref info-foo 'locals))
      ;; new-frames, locals, undead-out, call-undead, conflicts, assignment
      (= 6 (length info-foo))

      (empty? (info-ref info-bar 'assignment))
      (equal? '(tmp-ra.16) (info-ref info-bar 'locals))
      ;; new-frames, locals, undead-out, call-undead, conflicts, assignment
      (= 6 (length info-bar))

      (equal?
        ;; this might be a bit fragile...
        (list->set
          '((tmp-ra.17 fv0)
            (f.1 fv1)
            (e.2 fv2)
            (d.3 fv3)
            (c.4 fv4)
            (b.5 fv5)
            (a.6 fv6)
            (x.10 fv6)
            (foo1.7 fv7)
            (y.9 fv4)
            (z.8 fv2)))
        (list->set (info-ref info 'assignment)))
      ;; locals should be updated
      (equal?
        (list->set '(bar1.12 x.11 tmp1.14 tmp2.13))
        (list->set (info-ref info 'locals)))
      ;; new-frames, locals, undead-out, call-undead, conflicts, assignment
      (= 6 (length info))))
  )

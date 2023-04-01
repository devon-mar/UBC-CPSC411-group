#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7)

(provide allocate-frames)

;; Milestone 6 Exercise 11
;;
;; Compiles Asm-pred-lang-v7/pre-framed to Asm-pred-lang-v7/framed by
;; allocating frames for each non-tail call, and assigning all new-frame
;; variables to frame variables in the new frame.
(define/contract (allocate-frames p)
  (-> asm-pred-lang-v7/pre-framed? asm-pred-lang-v7/framed?)

  (define fbp (current-frame-base-pointer-register))
  (define assignment/c (listof (list/c aloc? fvar?)))
  (define frame-words? exact-nonnegative-integer?)

  ;; Removes 'call-undead and 'new-frames, updates 'assignment with as
  ;; and updates 'locals with 'ocals.
  ;;
  ;; p: asm-pred-lang-v7/pre-framed-info
  ;; -> asm-pred-lang-v7/framed-info
  (define/contract (pre-framed->framed-info info as locals)
    (-> info? assignment/c (listof aloc?) info?)
    (info-set
      (info-set
        (info-remove (info-remove (info-remove info 'call-undead) 'new-frames) 'undead-out)
        'locals
        locals)
      'assignment
      as))

  ;; If there is an entry in as for a, return the matching fvar.
  ;; Otherwise return a.
  (define/contract (aloc->fvar as a)
    (-> assignment/c aloc? (or/c aloc? fvar?))
    (let f ([as as])
      (cond
        [(empty? as) a]
        [(equal? (first (car as)) a) (second (car as))]
        [else (f (cdr as))])))

  ;; Returns the size of the frame in words from info.
  (define/contract (frame-size/words info)
    (-> info? exact-nonnegative-integer?)
    (define as (info-ref info 'assignment))
    (define co (info-ref info 'call-undead))
    (define fvar-indexes (map fvar->index (filter fvar? (map (lambda (a) (aloc->fvar as a)) co))))
    (define max-fv
      (if (empty? fvar-indexes)
        -1
        (foldl max (car fvar-indexes) (cdr fvar-indexes))))
    (max (length co) (+ 1 max-fv)))

  ;; Assign all new-frame variables to frame variables in the new frame.
  ;; Returns the new assignments.
  (define/contract (assign-nfvs fw info)
    (-> frame-words? info? (values assignment/c (listof aloc?)))

    ;; Assign each new-frame variable in a set of new-frame variables 
    ;; to a fvar. Returns new assignments and new locals.
    (define/contract (assign-nfv-set nfv-set acc)
      (-> (listof aloc?) assignment/c assignment/c)
      (define-values (as _)
        (for/fold ([acc acc]
                   [idx fw])
                  ([nfv nfv-set])
          (values
            (cons (list nfv (make-fvar idx)) acc)
            (add1 idx))))
      as)
    (values
      (foldl assign-nfv-set (info-ref info 'assignment) (info-ref info 'new-frames))
      (set-subtract
        (info-ref info 'locals)
        (apply append (info-ref info 'new-frames)))))

  ;; p: asm-pred-lang-v7/pre-framed-proc
  ;; -> asm-pred-lang-v7/framed-proc
  (define/contract (allocate-frames-proc label info tail)
    (-> label? info? any/c any/c)
    (define fw (frame-size/words info))
    (define-values (new-as new-locals) (assign-nfvs fw info))
    `(define
       ,label
       ,(pre-framed->framed-info info new-as new-locals)
       ,(allocate-frames-tail fw tail)))

  ;; p: asm-pred-lang-v7/pre-framed-p
  ;; -> asm-pred-lang-v7/framed-p
  (define (allocate-frames-p p)
    (match p
      [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
        (define fw (frame-size/words info))
        (define-values (new-as new-locals) (assign-nfvs fw info))
        `(module
           ,(pre-framed->framed-info info new-as new-locals)
           ,@(map allocate-frames-proc labels infos tails)
           ,(allocate-frames-tail fw tail))]))

  ;; p: asm-pred-lang-v7/pre-framed-pred
  ;; -> asm-pred-lang-v7/framed-pred
  (define (allocate-frames-pred fw p)
    (-> frame-words? any/c any/c)
    (match p
      [`(true)
        p]
      [`(false)
        p]
      [`(not ,p)
        `(not ,(allocate-frames-pred fw p))]
      [`(begin ,es ... ,p)
        `(begin
           ,@(map (lambda (e) (allocate-frames-effect fw e)) es)
           ,(allocate-frames-pred fw p))]
      [`(if ,p1 ,p2 ,p3)
        `(if
           ,(allocate-frames-pred fw p1)
           ,(allocate-frames-pred fw p2)
           ,(allocate-frames-pred fw p3))]
      [`(,_ ,_ ,_)
        p]))

  ;; t: asm-pred-lang-v7/pre-framed-tail
  ;; -> asm-pred-lang-v7/framed-tail
  (define/contract (allocate-frames-tail fw t)
    (-> frame-words? any/c any/c)
    (match t
      [`(jump ,_ ,_ ...)
        t]
      [`(begin ,es ... ,t)
        `(begin
           ,@(map (lambda (e) (allocate-frames-effect fw e)) es)
           ,(allocate-frames-tail fw t))]
      [`(if ,p ,t1 ,t2)
        `(if
           ,(allocate-frames-pred fw p)
           ,(allocate-frames-tail fw t1)
           ,(allocate-frames-tail fw t2))]))

  ;; e: asm-pred-lang-v7/pre-framed-effect
  ;; -> asm-pred-lang-v7/framed-effect
  (define (allocate-frames-effect fw e)
    (-> frame-words? any/c any/c)
    (match e
      [`(set! ,_ (,_ ,_ ,_))
        e]
      [`(set! ,_ ,_)
        e]
      ;; modified template - moved tail effect since we assume valid input.
      [`(begin ,es ...)
        `(begin ,@(map (lambda (e) (allocate-frames-effect fw e)) es))]
      [`(if ,p ,e1 ,e2)
        `(if
           ,(allocate-frames-pred fw p)
           ,(allocate-frames-effect fw e1)
           ,(allocate-frames-effect fw e2))]
      [`(return-point ,label ,tail)
        (define nb (* (current-word-size-bytes) fw))
        `(begin
           (set! ,fbp (- ,fbp ,nb))
           (return-point ,label ,tail)
           (set! ,fbp (+ ,fbp ,nb)))]))

  ;; not used
  #;
  (define (allocate-frames-opand o)
    (match o
      [(? int64?)
       (void)]
      [loc (void)]))

  ;; not used
  #;
  (define (allocate-frames-triv t)
    (match t
      [(? label?)
       (void)]
      [opand (void)]))

  ;; not used
  #;
  (define (allocate-frames-loc l)
    (match l
      [(? aloc?)
       (void)]
      [rloc (void)]))

  ;; not used
  #;
  (define (allocate-frames-trg t)
    (match t
      [(? label?)
       (void)]
      [loc (void)]))

  ;; not used
  #;
  (define (allocate-frames-binop b)
    (match b
      ['* (void)]
      ['+ (void)]
      ['- (void)]
      ['bitwise-and (void)]
      ['bitwise-ior (void)]
      ['bitwise-xor (void)]
      ['arithmetic-shift-right (void)]))
  
  ;; not used
  #;
  (define (allocate-frames-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

  (allocate-frames-p p))


(module+ test
  (require rackunit)

  (define (assignment<? a b)
    (symbol<? (first a) (first b)))

  #;
  (parameterize ([current-parameter-registers '()])
    (pretty-display
     ((compose
       assign-call-undead-variables
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
                 (let ([z.3 (call L.swap.1 y.2 x.1)])
                   z.3))))
         (call L.swap.1 1 2)))))


  ;; the following was generated from the above input to the interrogator
  (define expected-assignment-swap '((tmp-ra.1 fv2) (nfv.2 fv3) (nfv.3 fv4)))
  (check-match
    (allocate-frames
      '(module
         ((new-frames ())
          (locals (tmp-ra.4))
          (call-undead ())
          ;; the reference implementation of assign-call-undead-variables
          ;; doesn't seem to remove undead-out...
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
            (r15 (rbp fv0 fv1))))
          (assignment ()))
         (define L.swap.1
           ((new-frames ((nfv.2 nfv.3)))
            (locals (y.2 x.1 z.3 nfv.3 nfv.2))
            ;; the reference implementation of assign-call-undead-variables
            ;; doesn't seem to remove undead-out...
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
              (fv1 (x.1 tmp-ra.1))))
            (assignment ((tmp-ra.1 fv2))))
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
         ,swap-info
         (begin
           (set! tmp-ra.1 r15)
           (set! x.1 fv0)
           (set! y.2 fv1)
           (if (< y.2 x.1)
             (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
             (begin
               (begin
                 (set! rbp (- rbp 24))
                 (return-point L.rp.1
                   (begin
                     (set! nfv.3 x.1)
                     (set! nfv.2 y.2)
                     (set! r15 L.rp.1)
                     (jump L.swap.1 rbp r15 nfv.2 nfv.3)))
                 (set! rbp (+ rbp 24)))
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
      ;; locals, conflicts, assignment
      (= 3 (length info))
      (= 3 (length swap-info))
      (empty? (info-ref info 'assignment))
      (equal? (sort expected-assignment-swap assignment<?) (sort (info-ref swap-info 'assignment) assignment<?))
      ;; The rest of info and swap-info should remain unchanged
      (equal? (info-ref info 'locals) '(tmp-ra.4))
      (equal?
        (info-ref info 'conflicts)
        '((tmp-ra.4 (fv0 fv1 rbp))
            (rbp (r15 fv0 fv1 tmp-ra.4))
            (fv1 (r15 fv0 rbp tmp-ra.4))
            (fv0 (r15 rbp fv1 tmp-ra.4))
            (r15 (rbp fv0 fv1))))
      (equal? (sort (info-ref swap-info 'locals) symbol<?) '(x.1 y.2 z.3))
      (equal?
        (info-ref info 'conflicts)
        '((tmp-ra.4 (fv0 fv1 rbp))
            (rbp (r15 fv0 fv1 tmp-ra.4))
            (fv1 (r15 fv0 rbp tmp-ra.4))
            (fv0 (r15 rbp fv1 tmp-ra.4))
            (r15 (rbp fv0 fv1))))
      ))

  #;
  ((compose
     assign-call-undead-variables
     conflict-analysis
     undead-analysis
     uncover-locals
     select-instructions
     impose-calling-conventions
     normalize-bind
     sequentialize-let
     uniquify)
    '(module
       (define foo
         (lambda ()
           (if (if (true) (not (false)) (false))
             5
             0)))
       (define bar
         (lambda (x y)
           (if (let ([x (+ 1 x)]
                     [y (if (true) y 0)]) (= x y))
             x
             y)))
       (let ([a (call foo)]
             [b (call bar 36 36)])
         (+ a b))))

  (check-match
    (allocate-frames
      '(module
         ((new-frames (() ()))
          (locals (b.5))
          (call-undead (a.6 tmp-ra.9))
          (undead-out
           ((tmp-ra.9 rbp)
            ((rax tmp-ra.9 rbp) ((r15 rbp) (r15 rbp)))
            (a.6 tmp-ra.9 rbp)
            ((rax a.6 tmp-ra.9 rbp)
             ((rsi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
            (a.6 b.5 tmp-ra.9 rbp)
            (b.5 rax tmp-ra.9 rbp)
            (tmp-ra.9 rax rbp)
            (rax rbp)))
          (conflicts
           ((tmp-ra.9 (rax b.5 a.6 rbp))
            (a.6 (b.5 rbp tmp-ra.9))
            (b.5 (rax rbp tmp-ra.9 a.6))
            (rbp (rax b.5 rdi rsi a.6 r15 tmp-ra.9))
            (r15 (rdi rsi rbp))
            (rsi (r15 rdi rbp))
            (rdi (r15 rbp rsi))
            (rax (rbp tmp-ra.9 b.5))))
          (assignment ((tmp-ra.9 fv0) (a.6 fv1))))
         (define L.foo.1
           ((new-frames ())
            (locals (tmp-ra.7))
            (undead-out
             ((tmp-ra.7 rbp)
              (((tmp-ra.7 rbp) (tmp-ra.7 rbp) (tmp-ra.7 rbp))
               ((tmp-ra.7 rax rbp) (rax rbp))
               ((tmp-ra.7 rax rbp) (rax rbp)))))
            (call-undead ())
            (conflicts
             ((tmp-ra.7 (rbp rax)) (rax (rbp tmp-ra.7)) (rbp (tmp-ra.7 rax))))
            (assignment ()))
           (begin
             (set! tmp-ra.7 r15)
             (if (if (true) (not (false)) (false))
               (begin (set! rax 5) (jump tmp-ra.7 rbp rax))
               (begin (set! rax 0) (jump tmp-ra.7 rbp rax)))))
         (define L.bar.2
           ((new-frames ())
            (locals (x.4 y.3 x.2 y.1 tmp-ra.8))
            (undead-out
             ((rdi rsi tmp-ra.8 rbp)
              (rsi x.2 tmp-ra.8 rbp)
              (y.1 x.2 tmp-ra.8 rbp)
              ((((x.4 y.1 x.2 tmp-ra.8 rbp) (x.4 y.1 x.2 tmp-ra.8 rbp))
                ((x.4 y.1 x.2 tmp-ra.8 rbp)
                 (y.3 x.4 y.1 x.2 tmp-ra.8 rbp)
                 (y.3 x.4 y.1 x.2 tmp-ra.8 rbp))
                (y.1 x.2 tmp-ra.8 rbp))
               ((tmp-ra.8 rax rbp) (rax rbp))
               ((tmp-ra.8 rax rbp) (rax rbp)))))
            (call-undead ())
            (conflicts
             ((x.4 (y.3 rbp tmp-ra.8 x.2 y.1))
              (y.3 (rbp tmp-ra.8 x.2 y.1 x.4))
              (x.2 (y.1 rbp tmp-ra.8 rsi y.3 x.4))
              (y.1 (rbp tmp-ra.8 x.2 y.3 x.4))
              (tmp-ra.8 (y.1 x.2 rbp rsi rdi y.3 x.4 rax))
              (rax (rbp tmp-ra.8))
              (rbp (y.1 x.2 tmp-ra.8 y.3 x.4 rax))
              (rdi (tmp-ra.8))
              (rsi (x.2 tmp-ra.8))))
            (assignment ()))
           (begin
             (set! tmp-ra.8 r15)
             (set! x.2 rdi)
             (set! y.1 rsi)
             (if (begin
                   (begin (set! x.4 1) (set! x.4 (+ x.4 x.2)))
                   (if (true) (set! y.3 y.1) (set! y.3 0))
                   (= x.4 y.3))
               (begin (set! rax x.2) (jump tmp-ra.8 rbp rax))
               (begin (set! rax y.1) (jump tmp-ra.8 rbp rax)))))
         (begin
           (set! tmp-ra.9 r15)
           (return-point L.rp.3 (begin (set! r15 L.rp.3) (jump L.foo.1 rbp r15)))
           (set! a.6 rax)
           (return-point
            L.rp.4
            (begin
              (set! rsi 36)
              (set! rdi 36)
              (set! r15 L.rp.4)
              (jump L.bar.2 rbp r15 rdi rsi)))
           (set! b.5 rax)
           (set! rax a.6)
           (set! rax (+ rax b.5))
           (jump tmp-ra.9 rbp rax))))
    `(module
       ,info
       (define L.foo.1
         ,info-foo
         (begin
           (set! tmp-ra.7 r15)
           (if (if (true) (not (false)) (false))
             (begin (set! rax 5) (jump tmp-ra.7 rbp rax))
             (begin (set! rax 0) (jump tmp-ra.7 rbp rax)))))
       (define L.bar.2
         ,info-bar
         (begin
           (set! tmp-ra.8 r15)
           (set! x.2 rdi)
           (set! y.1 rsi)
           (if (begin
                 (begin (set! x.4 1) (set! x.4 (+ x.4 x.2)))
                 (if (true) (set! y.3 y.1) (set! y.3 0))
                 (= x.4 y.3))
             (begin (set! rax x.2) (jump tmp-ra.8 rbp rax))
             (begin (set! rax y.1) (jump tmp-ra.8 rbp rax)))))
       (begin
         (set! tmp-ra.9 r15)
         (begin
           (set! rbp (- rbp 16))
           (return-point L.rp.3 (begin (set! r15 L.rp.3) (jump L.foo.1 rbp r15)))
           (set! rbp (+ rbp 16)))
         (set! a.6 rax)
         (begin
           (set! rbp (- rbp 16))
           (return-point
            L.rp.4
            (begin
              (set! rsi 36)
              (set! rdi 36)
              (set! r15 L.rp.4)
              (jump L.bar.2 rbp r15 rdi rsi)))
           (set! rbp (+ rbp 16)))
         (set! b.5 rax)
         (set! rax a.6)
         (set! rax (+ rax b.5))
         (jump tmp-ra.9 rbp rax)))
    (and
      (= 3 (length info))
      (= 3 (length info-bar))
      (= 3 (length info-foo))

      (empty? (info-ref info-foo 'assignment))
      (empty? (info-ref info-bar 'assignment))
      (equal? (sort '((tmp-ra.9 fv0) (a.6 fv1)) assignment<?) (sort (info-ref info 'assignment) assignment<?))))

  ;; Identity over bitwise binops
  (check-equal?
    (allocate-frames
      `(module
        ((new-frames (() ()))
          (locals ())
          (call-undead ())
          (undead-out ())
          (conflicts ())
          (assignment ()))
        (begin
          (set! x.1 1)
          (set! x.2 0)
          (set! x.1 (bitwise-ior x.1 x.2))
          (set! x.1 (bitwise-xor x.1 x.2))
          (set! x.1 (bitwise-and x.1 x.2))
          (set! x.1 (arithmetic-shift-right x.1 x.2))
          (jump r15))))
    `(module
        ((locals ())
         (conflicts ())
         (assignment ()))
        (begin
          (set! x.1 1)
          (set! x.2 0)
          (set! x.1 (bitwise-ior x.1 x.2))
          (set! x.1 (bitwise-xor x.1 x.2))
          (set! x.1 (bitwise-and x.1 x.2))
          (set! x.1 (arithmetic-shift-right x.1 x.2))
          (jump r15))))
  )
#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v6)

(provide impose-calling-conventions)

;; Milestone 5 Exercise 5
;; Milestone 6 Exercise 5
;;
;; Compiles Proc-imp-cmf-lang v6 to Imp-cmf-lang v6 by imposing calling
;; conventions on all calls (both tail and non-tail calls), and entry points.
;; The registers used to passing parameters are defined by
;; current-parameter-registers, and the registers used for returning are
;; defined by current-return-address-register and current-return-value-register.
(define (impose-calling-conventions p)
  (-> proc-imp-cmf-lang-v6? imp-cmf-lang-v6?)

  ;; for convenience...
  (define ra (current-return-address-register))
  (define rv (current-return-value-register))
  (define fbp (current-frame-base-pointer-register))


  ;; Returns a new empty box for storing lists of new frame vars.
  (define/contract (make-nfvs-box)
    (-> box?)
    (box '()))

  ;; Add nfvs to nfvs-box.
  (define/contract (add-nfvs! nfvs-box nfvs)
    (-> box? (listof aloc?) void)
    (set-box!
      nfvs-box
      (cons
        nfvs
        (unbox nfvs-box))))

  ;; Return n alocs to be used to pass arguments for a call in non-tail position.
  ;; Any new fvars will be at the end of the list.
  ;; The list of used fvars will be added to nfvs-box.
  ;;
  ;; -> (listof imp-cmf-lang-v5-loc)
  (define/contract (args->locs/non-tail nfvs-box n)
    (-> box? exact-nonnegative-integer? (listof (or/c register? aloc?)))
    (define-values (locs nfvs)
      (let f ([n n]
              [regs (current-parameter-registers)]
              [next-fv 0])
        (cond
          [(zero? n) (values '() '())]
          [(empty? regs)
           (define-values (rest-locs rest-nfvs) (f (sub1 n) regs (add1 next-fv)))
           (define nfv (fresh 'nfv))
           (values
             (cons nfv rest-locs)
             (cons nfv rest-nfvs))]
          [else
            (define-values (rest-locs rest-nfvs) (f (sub1 n) (cdr regs) next-fv))
            (values
              (cons (car regs) rest-locs)
              rest-nfvs)])))
    (add-nfvs! nfvs-box nfvs)
    locs)

  ;; Return n alocs to be used to pass arguments for a call in tail position.
  ;; Any fvars will be at the end of the list.
  ;;
  ;; -> (listof imp-cmf-lang-v5-loc)
  (define/contract (args->locs/tail n)
    (-> exact-nonnegative-integer? (listof (or/c register? fvar?)))
    (let f ([n n]
            [regs (current-parameter-registers)]
            [next-fv 0])
      (cond
        [(zero? n) '()]
        [(empty? regs)
         (cons
           (make-fvar next-fv)
           (f (sub1 n) regs (add1 next-fv)))]
        [(cons
           (car regs)
           (f (sub1 n) (cdr regs) next-fv))])))

  ;; Maps each paramameter to a loc. All frame variable locs (if any) will be
  ;; at the end of the list.
  ;;
  ;; (listof proc-imp-cmf-lang-v5-aloc) -> (dict proc-imp-cmf-lang-v5-aloc -> imp-cmf-lang-v5-loc)
  (define/contract (params->locs p)
    (-> (listof aloc?) dict?)
    (let f ([p p]
            [regs (current-parameter-registers)]
            [next-fv 0]
            [acc '()])
      (cond
        [(empty? p) acc]
        [(empty? regs)
         (f 
           (cdr p)
           regs
           (add1 next-fv)
           (dict-set acc (car p) (make-fvar next-fv)))]
        [(f
           (cdr p)
           (cdr regs)
           next-fv
           (dict-set acc (car p) (car regs)))])))

  ;; Imposes callign conventions on a proc represented by label params and entry.
  ;;
  ;; entry: proc-imp-cmf-lang-v6-entry
  ;; -> imp-cmf-lang-v6-proc
  (define/contract (impose-calling-conventions-proc label params entry)
    (-> label? (listof aloc?) any/c any/c)
    (define locs (params->locs params))
    (define nfvs-box (make-nfvs-box))
    (define tail
      (impose-calling-conventions-entry
        nfvs-box
        (make-begin
          (map (lambda (p) `(set! ,p ,(dict-ref locs p))) params)
          entry)))
    `(define
       ,label
       ,(info-set '() 'new-frames (unbox nfvs-box))
       ,tail))

  ;; proc-imp-cmf-lang-v6-p -> imp-cmf-lang-v6-p
  (define (impose-calling-conventions-p p)
    (match p
      [`(module (define ,labels (lambda (,alocs ...) ,entries)) ... ,entry)
        (define nfvs-box (make-nfvs-box))
        (define tail (impose-calling-conventions-entry nfvs-box entry))
        `(module
           ,(info-set '() 'new-frames (unbox nfvs-box))
           ,@(map impose-calling-conventions-proc labels alocs entries)
           ,tail)]))

  ;; nfvs-box: box? of nfvs
  ;; e: proc-imp-cmf-lang-v6-entry
  ;; -> imp-cmf-lang-v6-tail
  (define/contract (impose-calling-conventions-entry nfvs-box e)
    (-> box? any/c any/c)
    (define tmp-ra (fresh 'tmp-ra))
    `(begin
       (set! ,tmp-ra ,(current-return-address-register))
       ,(impose-calling-conventions-tail nfvs-box tmp-ra e)))

  ;; nfvs-box: box? of nfvs
  ;; p proc-imp-cmf-lang-v6-pred
  ;; -> imp-cmf-lang-v6-pred
  (define/contract (impose-calling-conventions-pred nfvs-box p)
    (-> box? any/c any/c)
    (match p
      [`(true)
        p]
      [`(false)
        p]
      [`(not ,p)
        `(not ,(impose-calling-conventions-pred nfvs-box p))]
      [`(begin ,es ... ,p)
        `(begin
           ,@(map (lambda (e) (impose-calling-conventions-effect nfvs-box e)) es)
           ,(impose-calling-conventions-pred nfvs-box p))]
      [`(if ,p1 ,p2 ,p3)
        `(if
           ,(impose-calling-conventions-pred nfvs-box p1)
           ,(impose-calling-conventions-pred nfvs-box p2)
           ,(impose-calling-conventions-pred nfvs-box p3))]
      [`(,_ ,_ ,_)
        p]))

  ;; nfvs-box: box? of nfvs
  ;; tmp-ra: aloc? holding the return address
  ;; t: proc-imp-cmf-lang-v6-tail
  ;; -> imp-cmf-lang-v6-tail
  (define/contract (impose-calling-conventions-tail nfvs-box tmp-ra t)
    (-> box? aloc? any/c any/c)
    (match t
      [`(call ,t ,os ...)
        (define locs (args->locs/tail (length os)))
        `(begin
           ,@(reverse (map (lambda (a l) `(set! ,l ,a)) os locs))
           (set! ,ra ,tmp-ra)
           (jump ,t ,fbp ,ra ,@locs))]
      [`(begin ,es ... ,t)
        `(begin
           ,@(map (lambda (e) (impose-calling-conventions-effect nfvs-box e)) es)
           ,(impose-calling-conventions-tail nfvs-box tmp-ra t))]
      [`(if ,p ,t1 ,t2)
        `(if
           ,(impose-calling-conventions-pred nfvs-box p)
           ,(impose-calling-conventions-tail nfvs-box tmp-ra t1)
           ,(impose-calling-conventions-tail nfvs-box tmp-ra t2))]
      [_
        (impose-calling-conventions-value
          nfvs-box
          t
          (lambda (v)
            `(begin
               (set! ,rv ,v)
               (jump ,tmp-ra ,fbp ,rv))))]))

  ;; nfvs-box: box? of nfvs
  ;; v: proc-imp-cmf-lang-v6-value
  ;; f: (value -> tail/effect)
  ;; -> tail/effect
  (define/contract (impose-calling-conventions-value nfvs-box v f)
    (-> box? any/c (-> any/c any/c) any/c)
    (match v
      [`(call ,t ,os ...)
        (define rp-label (fresh-label 'rp))
        (define locs (args->locs/non-tail nfvs-box (length os)))
        `(begin
           (return-point
             ,rp-label
             (begin
               ,@(reverse (map (lambda (a l) `(set! ,l ,a)) os locs))
               (set! ,ra ,rp-label)
               (jump ,t ,fbp, ra ,@locs)))
           ,(f rv))]
      [`(,_ ,_ ,_)
        (f v)]
      [_ (f v)]))

  ;; nfvs-box: box? of nfvs
  ;; e: proc-imp-cmf-lang-v6-effect
  ;; -> imp-cmf-lang-v6-effect
  (define/contract (impose-calling-conventions-effect nfvs-box e)
    (-> box? any/c any/c)
    (match e
      [`(set! ,a ,v)
        (impose-calling-conventions-value
          nfvs-box
          v
          (lambda (v) `(set! ,a ,v)))]
      ;; modified template - removed tail e since we assume valid input
      [`(begin ,es ...)
        `(begin ,@(map (lambda (e) (impose-calling-conventions-effect nfvs-box e)) es))]
      [`(if ,p ,e1 ,e2)
        `(if
           ,(impose-calling-conventions-pred nfvs-box p)
           ,(impose-calling-conventions-effect nfvs-box e1)
           ,(impose-calling-conventions-effect nfvs-box e2))]))

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
      ['+ (void)]
      ['- (void)]))

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
      (interp-imp-cmf-lang-v6 (impose-calling-conventions p))
      42))

  (define/contract (alocs? . as)
    (-> any/c ... boolean?)
    (and
      (andmap aloc? as)
      (not (check-duplicates as))))

  (define/contract (labels? .  ls)
    (-> any/c ... boolean?)
    (and
      (andmap label? ls)
      (not (check-duplicates ls))))

  (define/contract (valid-current-regs ra rbp rax)
    (-> any/c any/c any/c boolean?)
    (and
      (equal? ra (current-return-address-register))
      (equal? rbp (current-frame-base-pointer-register))
      (equal? rax (current-return-value-register))))

  (define/contract (check-list-lengths lols lens)
    (-> (listof (listof any/c)) (listof exact-nonnegative-integer?) boolean?)
    (if (= (length lols) (length lens))
      (let f ([lols lols]
              [lens lens])
        (cond
          [(empty? lols)
           #t]
          [(= (length (car lols)) (car lens))
           (f (cdr lols) (cdr lens))]
          [else #f]))
      #f))
  (check-true (check-list-lengths '() '()))
  (check-false (check-list-lengths '() '(1)))
  (check-false (check-list-lengths '((1) ()) '(1)))
  (check-true (check-list-lengths '(() (1) (1 2) (1 2 3)) '(0 1 2 3)))
  (check-false (check-list-lengths '((1) (1 2) (1 2 3)) '(1 0 3)))
          


  ;; tail/value
  (check-42 '(module 42))
  ;; let's make sure we're actually doing the right thing
  (check-match
    (impose-calling-conventions '(module 42))
    `(module
       ((new-frames ()))
       (begin
         (set! ,tmp-ra ,r15)
         (begin
           (set! rax 42)
           (jump ,tmp-ra ,rbp ,rax))))
    (and (alocs? tmp-ra)
         (valid-current-regs r15 rbp rax)))
  (check-42
    '(module
       (begin
         (set! x.1 42)
         x.1)))

  ;; value/call
  (define prog1 
    '(module
       (define L.foo.1 (lambda () 42))
       (begin
         (set! x.1 (call L.foo.1))
         x.1)))
  (check-42 prog1)
  (check-match
    (impose-calling-conventions prog1)
    `(module
       ((new-frames (())))
       (define L.foo.1
         ((new-frames ()))
         (begin
           (set! ,tmp-ra1 ,r15)
           (begin (set! rax 42) (jump ,tmp-ra1 ,rbp ,rax))))
       (begin

         (set! ,tmp-ra2 ,r15)
         (begin
           (begin
             (return-point ,rp-label (begin (set! ,r15 ,rp-label) (jump L.foo.1 ,rbp ,r15)))
             (set! x.1 rax))
           (begin
             (set! rax x.1)
             (jump ,tmp-ra2 ,rbp ,rax)))))
    (and (alocs? tmp-ra1 tmp-ra2)
         (labels? rp-label)
         (valid-current-regs r15 rbp rax)))

  ;; lots of tail calls no args
  (check-42
    '(module
       (define L.baz.1 (lambda () 42))
       (define L.bar.1 (lambda () (call L.baz.1)))
       (define L.foo.1 (lambda () (call L.bar.1)))
       (call L.foo.1)))

  (check-42 '(module (define L.foo.1 (lambda () 42)) (call L.foo.1)))

  ;; 1 arg in tail position
  (check-42 '(module (define L.foo.1 (lambda (x.1) x.1)) (call L.foo.1 42)))

  ;; 1 arg in non-tail position
  (check-42
    '(module
       (define L.foo.1 (lambda (x.1) x.1))
       (begin
         (set! x.1 (call L.foo.1 42))
         x.1)))

  ;; test args in fvars by forcing spill into fvars for args in non-tail
  (define prog2
    '(module
       (define L.bar.1 (lambda (x.1 x.2) (+ x.1 x.2)))
       (define L.foo.1 (lambda (x.1) x.1))
       (begin
         (set! x.1 (call L.foo.1 42))
         (set! x.2 (call L.bar.1 10 20))
         x.1)))
  (parameterize ([current-parameter-registers '()])
    (check-42 prog2)
    (check-match
      (impose-calling-conventions prog2)
      `(module
         ((new-frames ((,nfv2 ,nfv1) (,nfv0))))
         (define L.bar.1
           ((new-frames ()))
           (begin
             (set! ,tmp-ra2 ,r15)
             (begin
               (set! x.1 fv0)
               (set! x.2 fv1)
               (begin
                 (set! ,rax (+ x.1 x.2))
                 (jump ,tmp-ra2 ,rbp ,rax)))))
         (define L.foo.1
           ((new-frames ()))
           (begin
             (set! ,tmp-ra1 ,r15)
               (begin
                 (set! x.1 fv0)
                 (begin
                   (set! ,rax x.1)
                   (jump ,tmp-ra1 ,rbp ,rax)))))
         (begin
           (set! ,tmp-ra0 ,r15)
           (begin
             (begin
               (return-point
                 ,rp-label0
                 (begin
                   (set! ,nfv0 42)
                   (set! ,r15 ,rp-label0)
                   (jump L.foo.1 ,rbp ,r15 ,nfv0)))
               (set! x.1 ,rax))
             (begin
               (return-point
                ,rp-label1
                (begin
                  (set! ,nfv1 20)
                  (set! ,nfv2 10)
                  (set! r15 ,rp-label1)
                  (jump L.bar.1 ,rbp ,r15 ,nfv2 ,nfv1)))
               (set! x.2 rax))
             (begin (set! ,rax x.1) (jump ,tmp-ra0 ,rbp ,rax)))))
      (and (alocs? tmp-ra0 tmp-ra1 tmp-ra2 nfv0 nfv1 nfv2)
           (labels? rp-label0 rp-label1)
           (valid-current-regs r15 rbp rax))))

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

  ;; 10 args tail call
  (check-match
    (impose-calling-conventions
      '(module
         (define L.foo.1 (lambda (x.0 x.1 x.2 x.3 x.4 x.5 x.6 x.7 x.8 x.9) 42))
         (call L.foo.1 0 1 2 3 4 5 6 7 8 9)))
    `(module
       ((new-frames ()))
       (define L.foo.1
         ((new-frames ()))
         (begin
           (set! ,tmp-ra1 ,r15)
           (begin
             (set! x.0 ,p0)
             (set! x.1 ,p1)
             (set! x.2 ,p2)
             (set! x.3 ,p3)
             (set! x.4 ,p4)
             (set! x.5 ,p5)
             (set! x.6 ,p6)
             (set! x.7 ,p7)
             (set! x.8 ,p8)
             (set! x.9 ,p9)
             (begin
               (set! rax 42)
               (jump ,tmp-ra1 ,rbp ,rax)))))
       (begin
         (set! ,tmp-ra0 r15)
         (begin
           (set! ,a9 9)
           (set! ,a8 8)
           (set! ,a7 7)
           (set! ,a6 6)
           (set! ,a5 5)
           (set! ,a4 4)
           (set! ,a3 3)
           (set! ,a2 2)
           (set! ,a1 1)
           (set! ,a0 0)
           (set! ,r15 ,tmp-ra0)
           (jump L.foo.1 ,rbp ,r15 ,a0 ,a1 ,a2 ,a3 ,a4 ,a5 ,a6 ,a7 ,a8 ,a9))))
    (and (valid-current-regs r15 rbp rax)
         (let ([params (list p0 p1 p2 p3 p4 p5 p6 p7 p8 p9)]
               [args (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)]
               [reg-count (length (current-parameter-registers))])
           (and
             (alocs? tmp-ra0 tmp-ra1)
             (equal? (take params reg-count) (current-parameter-registers))
             (equal? (take args reg-count) (current-parameter-registers))
             (andmap fvar? (drop args reg-count))
             (andmap fvar? (drop params reg-count))
             (equal? (range 0 (- (length args) reg-count)) (map fvar->index (drop args reg-count)))
             (equal? (range 0 (- (length params) reg-count)) (map fvar->index (drop params reg-count)))))))
  ;; 10 args non-tail call
  (check-match
    (impose-calling-conventions
      '(module
         (define L.foo.1 (lambda (x.0 x.1 x.2 x.3 x.4 x.5 x.6 x.7 x.8 x.9) 42))
         (begin
           (set! x.1 (call L.foo.1 0 1 2 3 4 5 6 7 8 9))
           x.1)))
    `(module
       ((new-frames ((,a6 ,a7 ,a8 ,a9))))
       (define L.foo.1
         ((new-frames ()))
         (begin
           (set! ,tmp-ra1 ,r15)
           (begin
             (set! x.0 ,p0)
             (set! x.1 ,p1)
             (set! x.2 ,p2)
             (set! x.3 ,p3)
             (set! x.4 ,p4)
             (set! x.5 ,p5)
             (set! x.6 ,p6)
             (set! x.7 ,p7)
             (set! x.8 ,p8)
             (set! x.9 ,p9)
             (begin
               (set! rax 42)
               (jump ,tmp-ra1 ,rbp ,rax)))))
       (begin
         (set! ,tmp-ra0 r15)
         (begin
           (begin
             (return-point
              ,rp0
              (begin
                (set! ,a9 9)
                (set! ,a8 8)
                (set! ,a7 7)
                (set! ,a6 6)
                (set! ,a5 5)
                (set! ,a4 4)
                (set! ,a3 3)
                (set! ,a2 2)
                (set! ,a1 1)
                (set! ,a0 0)
                (set! ,r15 ,rp0)
                (jump L.foo.1 ,rbp ,r15 ,a0 ,a1 ,a2 ,a3 ,a4 ,a5 ,a6 ,a7 ,a8 ,a9)))
             (set! x.1 ,rax))
           (begin (set! ,rax x.1) (jump ,tmp-ra0 ,rbp ,rax)))))
    (and
      (alocs? tmp-ra0 tmp-ra1)
      (labels? rp0)
      (valid-current-regs r15 rbp rax)
      (let ([params (list p0 p1 p2 p3 p4 p5 p6 p7 p8 p9)]
            [args (list a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)]
            [reg-count (length (current-parameter-registers))])
        (and
          (alocs? tmp-ra0 tmp-ra1)
          (equal? (take params reg-count) (current-parameter-registers))
          (equal? (take args reg-count) (current-parameter-registers))
          (andmap aloc? (drop args reg-count))
          (andmap fvar? (drop params reg-count))
          (not (check-duplicates args))
          (equal? (range 0 (- (length params) reg-count)) (map fvar->index (drop params reg-count)))))))

  ;; Check the order of new-frames
  (check-match
    (impose-calling-conventions
      '(module
         (define L.ten.0 (lambda (x.0 x.1 x.2 x.3 x.4 x.5 x.6 x.7 x.8 x.9) x.9))
         (define L.nine.0 (lambda (x.0 x.1 x.2 x.3 x.4 x.5 x.6 x.7 x.8) x.8))
         (define L.eight.0 (lambda (x.0 x.1 x.2 x.3 x.4 x.5 x.6 x.7) x.7))
         (define L.seven.0 (lambda (x.0 x.1 x.2 x.3 x.4 x.5 x.6) x.6))
         (define L.six.0 (lambda (x.0 x.1 x.2 x.3 x.4 x.5) x.5))
         (define L.five.0 (lambda (x.0 x.1 x.2 x.3 x.4) x.4))
         (define L.foo.1
           (lambda ()
             (begin
               (if
                 (begin
                   (set! x.0 (call L.ten.0 0 1 2 3 4 5 6 7 8 9))
                   (set! x.1 (call L.nine.0 0 1 2 3 4 5 6 7 8))
                   (true))
                 (begin
                   (set! x.2 (call L.eight.0 0 1 2 3 4 5 6 7))
                   (set! x.3 (call L.seven.0 0 1 2 3 4 5 6)))
                 (set! x.4 (call L.six.0 0 1 2 3 4 5)))
               (set! x.5 (call L.five.0 0 1 2 3 4))
               (set! x.6 (call L.seven.0 0 1 2 3 4 5 6))
               (call L.five.0 0 1 2 3 4))))
         (begin
           (if
             (begin
               ;; 4
               (set! x.0 (call L.ten.0 0 1 2 3 4 5 6 7 8 9))
               ;; 3
               (set! x.1 (call L.nine.0 0 1 2 3 4 5 6 7 8))
               (true))
             (begin
               ;; 2
               (set! x.2 (call L.eight.0 0 1 2 3 4 5 6 7))
               ;; 1
               (set! x.3 (call L.seven.0 0 1 2 3 4 5 6)))
             ;; 0
             (set! x.4 (call L.six.0 0 1 2 3 4 5)))
           ;; 0
           (set! x.5 (call L.five.0 0 1 2 3 4))
           ;; 1
           (set! x.6 (call L.seven.0 0 1 2 3 4 5 6))
           (call L.five.0 0 1 2 3 4))))
    `(module
       ,info
       (define L.ten.0 ((new-frames ())) ,_)
       (define L.nine.0 ((new-frames ())) ,_)
       (define L.eight.0 ((new-frames ())),_)
       (define L.seven.0 ((new-frames ())),_)
       (define L.six.0 ((new-frames ())),_)
       (define L.five.0 ((new-frames ())),_)
       (define L.foo.1 ,info-foo ,_)
       ,_)
    (and
      (check-list-lengths (info-ref info 'new-frames) '(1 0 0 1 2 3 4))
      (check-list-lengths (info-ref info-foo 'new-frames) '(1 0 0 1 2 3 4))
      (not (check-duplicates (apply append (info-ref info 'new-frames))))
      (not (check-duplicates (apply append (info-ref info-foo 'new-frames))))))

  )

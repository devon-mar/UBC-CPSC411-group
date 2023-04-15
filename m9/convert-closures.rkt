#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/info-lib
  cpsc411/langs/v9)

(provide convert-closures)

;; Milestone 9 Exercise 9
;;
;; Performs closure conversion,
;; converting all procedures into explicit closures.
(define/contract (convert-closures p)
  (-> lam-free-lang-v9? closure-lang-v9?)

  ;; lam-free-lang-v9-p -> closure-lang-v9-p
  (define (convert-closures-p p)
    (match p
      [`(module ,value)
       `(module ,(convert-closures-value value))]))

  ;; lam-free-lang-v9-value -> closure-lang-v9-value
  (define (convert-closures-value v)
    (match v
      [`(unsafe-procedure-call ,vc ,vs ...)
       (define new-vs (map convert-closures-value vs))
       (if (aloc? vc)
           `(closure-call ,vc ,vc ,@new-vs)
           (let ([tmp (fresh)]
                 [nvc (convert-closures-value vc)])
             `(let ([,tmp ,nvc])
                (closure-call ,tmp ,tmp ,@new-vs))))]
      [`(letrec ([,alocs (lambda ,infos (,params ...) ,vs)] ...) ,vt)
       (define labels (for/list ([a alocs]) (fresh-label a)))
       (define frees (for/list ([info infos]) (info-ref info 'free)))
       (define lambdas
         (for/list ([ps params] [v vs] [free frees])
           (define closure-aloc (fresh 'c))
           (define closure-refs
             (for/list ([i (range (length free))])
               `(closure-ref ,closure-aloc ,i)))
           `(lambda (,closure-aloc ,@ps)
              (let ,(map list free closure-refs)
                ,(convert-closures-value v)))))
       (define label-lambda-pairs (map list labels lambdas))
       (define closures
         (for/list ([ps params] [label labels] [free frees])
           `(make-closure ,label ,(length ps) ,@free)))
       `(letrec
          ,(map list labels lambdas)
          (cletrec ,(map list alocs closures)
            ,(convert-closures-value vt)))]
      [`(let ([,alocs ,vs] ...) ,vt)
       (define new-vs (map convert-closures-value vs))
       `(let
          ,(map list alocs new-vs)
          ,(convert-closures-value vt))]
      [`(if ,vp ,vt ,vf)
       `(if
          ,(convert-closures-value vp)
          ,(convert-closures-value vt)
          ,(convert-closures-value vf))]
      [`(begin ,effects ... ,value)
       `(begin
          ,@(map convert-closures-effect effects)
          ,(convert-closures-value value))]
      [`(,primop ,vs ...)
       #:when(primop? primop)
       (define new-vs (map convert-closures-value vs))
       `(,primop ,@new-vs)]
      [triv triv]))

  ;; lam-free-lang-v9-effect -> closure-lang-v9-effect
  (define (convert-closures-effect e)
    (match e
      [`(begin ,es ... ,et)
       `(begin
         ,@(map convert-closures-effect es)
         ,(convert-closures-effect et))]
      [`(,primop ,vs ...)
       `(,primop
         ,@(map convert-closures-value vs))]))

  ;; Unused
  #;
  (define (convert-closures-triv t)
    (match t
      [(? aloc?) (void)]
      [(? fixnum?) (void)]
      [#t (void)]
      [#f (void)]
      ['empty (void)]
      [`(void) (void)]
      [`(error _uint8) (void)]
      [(? ascii-char-literal?) (void)]))

  (define/contract (primop? p)
    (-> any/c boolean?)
    (and (memq p
               '(unsafe-fx*
                 unsafe-fx+
                 unsafe-fx-
                 eq?
                 unsafe-fx<
                 unsafe-fx<=
                 unsafe-fx>
                 unsafe-fx>=
                 fixnum?
                 boolean?
                 empty?
                 void?
                 ascii-char?
                 error?
                 not
                 pair?
                 vector?
                 procedure?
                 cons
                 unsafe-car
                 unsafe-cdr
                 unsafe-make-vector
                 unsafe-vector-length
                 unsafe-vector-set!
                 unsafe-vector-ref
                 unsafe-procedure-arity))
         #t))

  (convert-closures-p p))

(module+ test
  (require rackunit)

  ;; Check that the compiled program interprets to
  ;; the same value as the original
  (define-check (check-interp p)
    (define result (interp-closure-lang-v9 (convert-closures p)))
    (check-equal? result (interp-lam-free-lang-v9 p)))

  ;; Check that compiled program is the same as the original
  (define-check (check-no-change p)
    (check-equal? (convert-closures p) p))

  ;; base cases
  (check-no-change '(module 2))
  (check-no-change '(module (let () #t)))
  (check-no-change '(module (begin (error 2))))
  (check-equal?
    (convert-closures '(module (letrec () (void))))
    '(module (letrec () (cletrec () (void)))))

  ;; basic letrec + unsafe-procedure-call (2 param + 2 free)
  (define p0
    `(module
      (let ([x.1 30]
            [y.1 -4])
        (letrec ([fn.1 (lambda ((free (x.1 y.1))) (a.1 b.1)
                          (unsafe-fx-
                            (unsafe-fx* x.1 b.1)
                            (unsafe-fx- a.1 y.1)))])
          (unsafe-procedure-call fn.1 7 12)))))
  (check-match
    (convert-closures p0)
    `(module
      (let ([x.1 30]
            [y.1 -4])
        (letrec ([,label (lambda (,ctmp a.1 b.1)
                           (let ([x.1 (closure-ref ,ctmp 0)]
                                 [y.1 (closure-ref ,ctmp 1)])
                             (unsafe-fx-
                               (unsafe-fx* x.1 b.1)
                               (unsafe-fx- a.1 y.1))))])
          (cletrec ([fn.1 (make-closure ,label 2 x.1 y.1)])
            (closure-call fn.1 fn.1 7 12)))))
    (and (label? label) (aloc? ctmp)))
  (check-interp p0)

  ;; letrec in unsafe-procedure-call
  (define p1
    `(module
       (unsafe-procedure-call
         (letrec ([fn.1 (lambda ((free ())) (x.1)
                         (unsafe-fx+ x.1 x.1))])
           fn.1)
         8)))
  (check-match
    (convert-closures p1)
    `(module
      (let ([,tmp (letrec ([,label (lambda (,ctmp x.1) (let () (unsafe-fx+ x.1 x.1)))])
                    (cletrec ([fn.1 (make-closure ,label 1)])
                      fn.1))])
         (closure-call ,tmp ,tmp 8)))
    (and (label? label)
         (andmap aloc? (list tmp ctmp))))
  (check-interp p1)

  ;; letrec in letrec (unsafe-procedure-call in unsafe-procedure-call)
  (check-interp
    `(module
      (letrec ([fn.1 (lambda ((free ())) ()
                      (letrec ([x.1 (lambda ((free (x.1))) (a.1)
                                      (if (unsafe-fx<= a.1 0)
                                          1
                                          (unsafe-fx*
                                            a.1
                                            (unsafe-procedure-call
                                              x.1
                                              (unsafe-fx- a.1 1)))))])
                        x.1))])
        (unsafe-procedure-call (unsafe-procedure-call fn.1) 5))))

  ;; letrec in letrec (1 param + 1 free in both)
  (check-interp
    `(module
      (let ([b.1 4])
        (letrec ([fn.1 (lambda ((free (b.1))) (a.1)
                         (letrec ([x.1 (lambda ((free (x.1))) (a.1)
                                         (if (unsafe-fx<= a.1 0)
                                              1
                                              (unsafe-fx*
                                                a.1
                                                (unsafe-procedure-call
                                                  x.1
                                                  (unsafe-fx- a.1 1)))))])
                           (unsafe-fx- (unsafe-procedure-call x.1 6) a.1)))])
          (unsafe-procedure-call fn.1 8)))))

  ;; many params
  (check-interp
    `(module
      (letrec ([fn.1 (lambda ((free ())) (a.1 b.1 c.1 d.1 e.1 f.1)
                       (unsafe-fx-
                         (unsafe-fx* a.1 b.1)
                         (unsafe-fx*
                           (unsafe-fx- c.1 d.1)
                           (unsafe-fx+ e.1 f.1))))])
        (unsafe-procedure-call fn.1 7 -12 5 -2 8 19))))

  ;; many frees
  (check-interp
    `(module
      (let ([a.1 -7] [b.1 12] [c.1 18] [d.1 -9] [e.1 2] [f.1 31])
        (letrec ([fn.1 (lambda ((free (a.1 b.1 c.1 d.1 e.1 f.1))) ()
                         (unsafe-fx-
                           (unsafe-fx* a.1 b.1)
                           (unsafe-fx*
                             (unsafe-fx- c.1 d.1)
                             (unsafe-fx+ e.1 f.1))))])
          (unsafe-procedure-call fn.1)))))

  ;; lets
  (check-interp
    `(module
      (let ([a.1 -7] [b.1 9])
        (let ([fn.1 (letrec ([fn.1 (lambda ((free (a.1))) (b.1)
                                (unsafe-fx- a.1 b.1))])
                      (let ([c.1 (unsafe-procedure-call fn.1 17)]) ;; -7 - 17 = -24
                        (letrec ([fn.1 (lambda ((free (b.1 c.1))) (d.1 e.1)
                                         (unsafe-fx-
                                           (unsafe-fx* b.1 c.1)     ;; 9 * -24 = -216
                                           (unsafe-fx+ d.1 e.1)))]) ;; 13 + -15 = -2
                          fn.1)))])
          (let ([b.1 13])
            (unsafe-procedure-call fn.1 b.1 (let ([b.1 -15]) b.1)))))))

  ;; multiple in letrec
  (check-interp
    `(module
      (let ([a.1 5] [b.1 8] [c.1 9])
        (letrec ([fn.0 (lambda ((free (a.1))) () a.1)]
                 [fn.1 (lambda ((free (c.1 b.1))) (x.1 y.1)
                         (unsafe-fx- c.1 (unsafe-fx+ x.1 (unsafe-fx* y.1 b.1))))]
                 [fn.2 (lambda ((free (fn.0 fn.1 c.1))) (x.1 y.1 z.1)
                         (unsafe-fx*
                           (unsafe-fx+ c.1 (unsafe-fx* x.1 (unsafe-procedure-call fn.0)))
                           (unsafe-procedure-call fn.1 y.1 z.1)))])
          (unsafe-fx-
            (unsafe-procedure-call fn.0)
            (unsafe-fx-
              (unsafe-procedure-call fn.1 3 -4)
              (unsafe-procedure-call fn.2 17 31 5)))))))

  ;; effects
  (check-interp
   `(module
      (let ([a.1 1]
            [b.1 2]
            [c.1 3]
            [d.1 4]
            [e.1 5]
            [v.1 (unsafe-make-vector 5)]
            [v.2 (unsafe-make-vector 10)]
            [v.3 (unsafe-make-vector 2)]
            [v.4 (unsafe-make-vector 6)])
        (begin
          (unsafe-vector-set! v.3 1 -9)
          (letrec ([fn.1 (lambda ((free (a.1 b.1 c.1 v.1 v.2 v.3))) (x.1 y.1)
                           (begin
                             (begin (begin (unsafe-vector-set! v.1 a.1 b.1)))
                             (begin
                               (unsafe-vector-set! v.4 2 (unsafe-vector-ref v.3 1))
                               (unsafe-vector-set! v.2 0 c.1)
                               (unsafe-vector-set! y.1 0 x.1)
                               (unsafe-vector-length v.3))))])
            (begin
              (unsafe-vector-set! v.3 1 (unsafe-fx* (unsafe-vector-ref v.3 1) 2))
              (begin (unsafe-vector-set! v.4 5 (unsafe-procedure-call fn.1 5 v.4)))
              (if (eq? (unsafe-vector-ref v.1 a.1) (unsafe-vector-ref v.4 5))
                  (if (eq? (unsafe-vector-ref v.4 0) 5)
                      (if (eq? (unsafe-vector-ref v.2 0) 3)
                          (unsafe-vector-ref v.4 2)
                          #f)
                      #f)
                  #f)))))))
  )

#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/info-lib
  cpsc411/langs/v9)

(provide uncover-free)

;; Milestone 9 Exercise 8
;;
;; Explicitly annotate procedures with their free variable sets.
(define/contract (uncover-free p)
  (-> lam-opticon-lang-v9? lam-free-lang-v9?)

  ;; lam-opticon-lang-v9-p -> lam-free-lang-v9-p
  (define (uncover-free-p p)
    (match p
      [`(module ,value)
       (define-values (new-val _)
         (uncover-free-value value '()))
       `(module ,new-val)]))

  ;; lam-opticon-lang-v9-value (listof aloc)
  ;; -> lam-free-lang-v9-value (listof aloc)
  ;; Given value and bound alocs,
  ;; return new value w/ info in letrecs and free alocs
  (define (uncover-free-value v bound)
    (match v
      [`(unsafe-procedure-call ,vc ,vs ...)
       (define-values (new-vc vc-fa)
         (uncover-free-value vc bound))
       (define-values (new-vs vs-fas)
         (map2 (lambda (v) (uncover-free-value v bound)) vs))
       (values
         `(unsafe-procedure-call ,new-vc ,@new-vs)
         (apply set-union vc-fa vs-fas))]
      [`(letrec ([,alocs (lambda (,params ...) ,vs)] ...) ,vt)
       (define-values (aloc-lambda-pairs vs-fas)
         (map2 (lambda (aloc ps v)
                 (define-values (new-v v-fa) (uncover-free-value v ps))
                 (define new-fa (set-subtract v-fa ps))
                 (define info (info-set '() 'free new-fa))
                 (values
                   `(,aloc (lambda ,info ,ps ,new-v))
                   new-fa))
           alocs params vs))
       (define new-bound (set-union bound alocs))
       (define-values (new-vt vt-fa)
         (uncover-free-value vt new-bound))
       (define new-fa (apply set-union (set-subtract vt-fa alocs) vs-fas))
       (values
         `(letrec ,aloc-lambda-pairs ,new-vt)
         new-fa)]
      [`(let ([,alocs ,vs] ...) ,vt)
       (define-values (new-vs vs-fas)
         (map2 (lambda (v) (uncover-free-value v bound)) vs))
       (define new-bound (set-union bound alocs))
       (define-values (new-vt vt-fa)
         (uncover-free-value vt new-bound))
       (define new-fa (apply set-union (set-subtract vt-fa alocs) vs-fas))
       (values
         `(let ,(map list alocs new-vs) ,new-vt)
         new-fa)]
      [`(if ,vp ,vt ,vf)
       (define-values (new-vp vp-fa) (uncover-free-value vp bound))
       (define-values (new-vt vt-fa) (uncover-free-value vt bound))
       (define-values (new-vf vf-fa) (uncover-free-value vf bound))
       (values
         `(if ,new-vp ,new-vt ,new-vf)
         (set-union vp-fa vt-fa vf-fa))]
      [`(begin ,effects ... ,value)
       (define-values (new-efs ef-fas)
         (map2 (lambda (e) (uncover-free-effect e bound)) effects))
       (define-values (new-val val-fa)
         (uncover-free-value value bound))
       (values
         `(begin ,@new-efs ,new-val)
         (apply set-union val-fa ef-fas))]
      [`(,primop ,vs ...)
       #:when(primop? primop)
       (define-values (new-vs vs-fas)
         (map2 (lambda (v) (uncover-free-value v bound)) vs))
       (values
         `(,primop ,@new-vs)
         (apply set-union '() vs-fas))]
      [triv
       (values
         triv
         (uncover-free-triv triv bound))]))

  ;; lam-opticon-lang-v9-effect (listof aloc)
  ;; -> lam-free-lang-v9-effect (listof aloc)
  ;; Given effect and bound alocs,
  ;; return new effect w/ info in letrecs and free alocs
  (define (uncover-free-effect e bound)
    (match e
      [`(begin ,es ... ,et)
       (define-values (new-efs ef-fas)
         (map2 (lambda (e) (uncover-free-effect e bound)) (append es (list et))))
       (values
         `(begin ,@new-efs)
         (apply set-union '() ef-fas))]
      [`(,primop ,vs ...)
       (define-values (new-vs vs-fas)
         (map2 (lambda (v) (uncover-free-value v bound)) vs))
       (values
         `(,primop ,@new-vs)
         (apply set-union '() vs-fas))]))

  ;; lam-opticon-lang-v9-triv (listof aloc) -> (listof aloc)
  ;; Given triv and bound alocs, return the free alocs
  (define (uncover-free-triv t bound)
    (match t
      [(? aloc?)
       (if (set-member? bound t)
           (list)
           (list t))]
      [(? fixnum?) (list)]
      [#t (list)]
      [#f (list)]
      ['empty (list)]
      [`(void) (list)]
      [`(error _uint8) (list)]
      [(? ascii-char-literal?) (list)]))

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

  (uncover-free-p p))

(module+ test
  (require
    rackunit
    "../utils/gen-utils.rkt")

  ;; base cases
  (check-equal?
    (uncover-free '(module 2))
    '(module 2))

  ;; all bound
  (check-equal?
    (uncover-free
      `(module
        (letrec ([fn.0 (lambda () 1)]
                 [fn.1 (lambda (x.1 y.1)
                         (unsafe-fx+ x.1 y.1))]
                 [fn.2 (lambda (x.1 y.1 z.1)
                         (unsafe-fx* x.1 (unsafe-fx- y.1 z.1)))])
          (unsafe-procedure-call fn.1 1 2))))
    `(module
        (letrec ([fn.0 (lambda ((free ())) () 1)]
                 [fn.1 (lambda ((free ())) (x.1 y.1)
                         (unsafe-fx+ x.1 y.1))]
                 [fn.2 (lambda ((free ())) (x.1 y.1 z.1)
                         (unsafe-fx* x.1 (unsafe-fx- y.1 z.1)))])
          (unsafe-procedure-call fn.1 1 2))))

  ;; Check the info's free alocs are as expected
  ;; info (listof aloc) -> true or error
  (define (check-free? info expected)
    (define actual (info-ref info 'free))
    (or (list-equiv? actual expected)
        (error (format "Wrong free aloc:\nactual: ~a\nexpected: ~a" actual expected))))

  ;; free aloc in triv
  (check-match
    (uncover-free
      `(module
        (let ([a.1 1])
          (letrec ([fn.1 (lambda () a.1)])
            (unsafe-procedure-call fn.1)))))
    `(module
        (let ([a.1 1])
          (letrec ([fn.1 (lambda ,info1 () a.1)])
            (unsafe-procedure-call fn.1))))
    (check-free? info1 '(a.1)))

  ;; free aloc in primop
  (check-match
    (uncover-free
      `(module
        (let ([a.1 1]
              [b.1 2]
              [c.1 3])
          (letrec ([fn.1 (lambda (x.1)
                           (unsafe-fx+ a.1 (unsafe-fx- x.1 b.1)))])
            (unsafe-procedure-call fn.1 5)))))
    `(module
        (let ([a.1 1]
              [b.1 2]
              [c.1 3])
          (letrec ([fn.1 (lambda ,info1 (x.1)
                           (unsafe-fx+ a.1 (unsafe-fx- x.1 b.1)))])
            (unsafe-procedure-call fn.1 5))))
    (check-free? info1 '(a.1 b.1)))

  ;; free aloc in if
  (check-match
    (uncover-free
      `(module
        (let ([a.1 1]
              [b.1 2]
              [c.1 3]
              [d.1 (cons 5 6)]
              [e.1 4]
              [f.1 5])
          (letrec ([fn.1 (lambda (x.1)
                           (if (unsafe-fx< x.1 a.1)
                               (unsafe-car d.1)
                               (if (not (eq? -5 c.1))
                                   x.1
                                   e.1)))]
                   [fn.2 (lambda (x.1 y.1)
                           (if (not (fixnum? c.1))
                               (unsafe-procedure-call fn.1 b.1)
                               (unsafe-procedure-call fn.1 x.1)))])
            (unsafe-procedure-call fn.2 1 f.1)))))
    `(module
      (let ([a.1 1]
            [b.1 2]
            [c.1 3]
            [d.1 (cons 5 6)]
            [e.1 4]
            [f.1 5])
        (letrec ([fn.1 (lambda ,info1 (x.1)
                         (if (unsafe-fx< x.1 a.1)
                             (unsafe-car d.1)
                             (if (not (eq? -5 c.1))
                                  x.1
                                  e.1)))]
                 [fn.2 (lambda ,info2 (x.1 y.1)
                         (if (not (fixnum? c.1))
                             (unsafe-procedure-call fn.1 b.1)
                             (unsafe-procedure-call fn.1 x.1)))])
          (unsafe-procedure-call fn.2 1 f.1))))
    (and (check-free? info1 '(a.1 c.1 d.1 e.1))
         (check-free? info2 '(fn.1 b.1 c.1))))

  ;; free aloc in effect
  (check-match
    (uncover-free
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
          (letrec ([fn.1 (lambda (x.1 y.1)
                           (begin
                             (begin (begin (unsafe-vector-set! v.1 a.1 b.1)))
                             (begin
                               (unsafe-vector-set! v.2 0 c.1)
                               (unsafe-vector-set! y.1 0 x.1)
                               (unsafe-vector-length v.3))))])
            (unsafe-procedure-call fn.1 5 v.4)))))
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
        (letrec ([fn.1 (lambda ,info1 (x.1 y.1)
                         (begin
                           (begin (begin (unsafe-vector-set! v.1 a.1 b.1)))
                           (begin
                             (unsafe-vector-set! v.2 0 c.1)
                             (unsafe-vector-set! y.1 0 x.1)
                             (unsafe-vector-length v.3))))])
          (unsafe-procedure-call fn.1 5 v.4))))
    (check-free? info1 '(a.1 b.1 c.1 v.1 v.2 v.3)))

  ;; free aloc in let/letrec
  (check-match
    (uncover-free
      `(module
        (let ([a.1 1]
              [b.1 2]
              [c.1 3]
              [d.1 4]
              [e.1 5]
              [f.1 6]
              [g.1 7])
          (letrec ([fn.1 (lambda (x.1 y.1)
                           (let ([a.1 b.1]
                                 [c.1 (unsafe-fx> 5 d.1)])
                             (letrec ([fn.2 (lambda (x.1 z.1)
                                              (unsafe-fx+
                                                (unsafe-fx- e.1 x.1)
                                                (unsafe-fx* c.1 y.1)))])
                                (if #t
                                    (unsafe-procedure-call fn.2 0 1)
                                    (unsafe-fx+ g.1 x.1)))))])
            (unsafe-procedure-call fn.1 5 f.1)))))
    `(module
      (let ([a.1 1]
            [b.1 2]
            [c.1 3]
            [d.1 4]
            [e.1 5]
            [f.1 6]
            [g.1 7])
        (letrec ([fn.1 (lambda ,info1 (x.1 y.1)
                         (let ([a.1 b.1]
                               [c.1 (unsafe-fx> 5 d.1)])
                           (letrec ([fn.2 (lambda ,info2 (x.1 z.1)
                                            (unsafe-fx+
                                              (unsafe-fx- e.1 x.1)
                                              ;; y.1 is bound by the outer lambda
                                              (unsafe-fx* c.1 y.1)))])
                             (if #t
                                 (unsafe-procedure-call fn.2 0 1)
                                 (unsafe-fx+ g.1 x.1)))))])
          (unsafe-procedure-call fn.1 5 f.1))))
    (and (check-free? info1 '(b.1 d.1 e.1 g.1))
         (check-free? info2 '(c.1 e.1 y.1))))
  )

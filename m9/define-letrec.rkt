#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide define->letrec)

;; Milestone 9 Exercise 5
;;
;; Transform all top-level bindings into local bindings.
(define/contract (define->letrec p)
  (-> exprs-unsafe-lang-v9? just-exprs-lang-v9?)

  ;; exprs-unsafe-lang-v9-p just-exprs-lang-v9-p
  (define (define->letrec-p p)
    (match p
      [`(module (define ,alocs (lambda (,params ...) ,vs)) ... ,v)
        (if (empty? alocs)
          p
          `(module
             (letrec ,(map (lambda (a ps v) `[,a (lambda ,ps ,v)]) alocs params vs)
               ,v)))]))

  #;
  (define (define->letrec-value v)
    (match v
      [`(unsafe-procedure-call ,v ,vs ...)
        (void)]
      [`(let ([,as ,vs] ...) ,v)
        (void)]
      [`(if ,v1 ,v2 ,v3)
        (void)]
      [`(begin ,es ... ,v)
        (void)]
      [`(,primop ,vs ...)
        #:when (primop? primop)
        (void)]
      [triv (void)]))

  #;
  (define (define->letrec-effect e)
    (match e
      [`(begin ,es ... ,e)
        (void)]
      [`(,primop ,vs ...)
        (void)]))

  #;
  (define (define->letrec-triv t)
    (match t
      [#t
       (void)]
      [#f
       (void)]
      ['empty
       (void)]
      ['(void)
       (void)]
      [`(error ,uint8)
        (void)]
      [(? ascii-char-literal?)
       (void)]
      [`(lambda (,as ...) ,v)
        (void)]
      [(? aloc?)
       (void)]
      [(? fixnum?)
       (void)]))

  #;
  (define (define->letrec-primop p)
    (match p
      ['unsafe-fx*
       (void)]
      ['unsafe-fx+
       (void)]
      ['unsafe-fx-
       (void)]
      ['eq?
       (void)]
      ['unsafe-fx<
       (void)]
      ['unsafe-fx<=
       (void)]
      ['unsafe-fx>
       (void)]
      ['unsafe-fx>=
       (void)]
      ['fixnum?
       (void)]
      ['boolean?
       (void)]
      ['empty?
       (void)]
      ['void?
       (void)]
      ['ascii-char?
       (void)]
      ['error?
       (void)]
      ['not
       (void)]
      ['pair?
       (void)]
      ['vector?
       (void)]
      ['procedure?
       (void)]
      ['cons
       (void)]
      ['unsafe-car
       (void)]
      ['unsafe-cdr
       (void)]
      ['unsafe-make-vector
       (void)]
      ['unsafe-vector-length
       (void)]
      ['unsafe-vector-set!
       (void)]
      ['unsafe-vector-ref
       (void)]
      ['unsafe-procedure-arity
       (void)]))

  (define->letrec-p p))

(module+ test
  (require rackunit)

  (define-syntax (t stx)
    (syntax-case stx ()
      [(_ in out) #`(check-equal? (define->letrec 'in) 'out)]
      [(_ in) #`(t in in)]))

  (t (module 42))

  (t
    (module
      (define foo.1 (lambda () 42))
      (void))
    (module
      (letrec ([foo.1 (lambda () 42)])
        (void))))

  (t
    (module
      (define foo.1 (lambda () 42))
      (define bar.1 (lambda (x.1 y.1 z.1) 42))
      42)
    (module
      (letrec ([foo.1 (lambda () 42)]
               [bar.1 (lambda (x.1 y.1 z.1) 42)])
        42)))
  )

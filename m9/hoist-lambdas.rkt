#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide hoist-lambdas)

;; Milestone 9 Exercise 11
(define/contract (hoist-lambdas p)
  (-> closure-lang-v9? hoisted-lang-v9?)

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

  ;; closure-lang-v9-p -> hoisted-lang-v9-p
  (define (hoist-lambdas-p p)
    (match p
      [`(module ,value)
        `(module ,(hoist-lambdas-value value))]))

  ;; closure-lang-v9-value -> hoisted-lang-v9-value
  (define (hoist-lambdas-value v)
    (match v
      [`(closure-ref ,v1 ,v2)
        (void)]
      [`(closure-call ,v1 ,vs ...)
        (void)]
      ['(cletrec ([,alocs (make-closure ,labels ,arity ,vs ...)] ...) ,value)
        (void)]
      [`(closure-call ,v ,vs ...)
        (void)]
      [`(call ,v ,vs ...)
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
      [triv
        (void)]))

  ;; closure-lang-v9-effect -> hoisted-lang-v9-effect
  (define (hoist-lambdas-effect e)
    (match e
      [`(begin ,es ... ,e)
        (void)]
      [`(,primop ,vs ...)
        (void)]))

  ;; Unused
  #;
  (define (hoist-lambdas-triv t)
    (match t
      [#t
       (void)]
      [#f
       (void)]
      ['empty
       (void)]
      [`(error ,uint8)
        (void)]
      [(? ascii-char-literal?)
       (void)]
      [(? label?)
        (void)]
      [(? fixnum?)
       (void)]
      [(? aloc?)
       (void)]
      ['(void)
        (void)]))

  ;; Unused
  #;
  (define (hoist-lambdas-primop p)
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

  (hoist-lambdas-p p))

(module+ test
  (require rackunit)
)

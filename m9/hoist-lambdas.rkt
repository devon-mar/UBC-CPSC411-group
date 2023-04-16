#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide hoist-lambdas)

;; Milestone 9 Exercise 11
;; Hoists letrec values to the top-level definitions
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

  ;; Holds list of lambda definitions
  ;; ((label (aloc ...) value) ...)
  (define lambdas-to-hoist '())

  ;; closure-lang-v9-p -> hoisted-lang-v9-p
  (define (hoist-lambdas-p p)
    (match p
      [`(module ,value)
        (define new-value (hoist-lambdas-value value))
        `(module ,@(hoist-lambdas-proc) ,new-value)]))

  ;; Constructs lambdas from the lambdas-to-hoist environment variable
  ;; (void) -> ((define label (lambda (aloc ...) value)) ...)
  (define (hoist-lambdas-proc)
    (for/list
      ([lam lambdas-to-hoist])
      (match lam
        [`(,l ,a ,v)
          `(define ,l (lambda ,a ,v))])))

  ;; closure-lang-v9-value -> hoisted-lang-v9-value
  (define (hoist-lambdas-value v)
    (match v
      [`(letrec ([,labels (lambda ,alocs ,vs)] ...) ,value)
        (begin0 (hoist-lambdas-value value)
          (for
            ([l labels]
             [a alocs]
             [v vs])
             (set! lambdas-to-hoist (cons (list l a (hoist-lambdas-value v)) lambdas-to-hoist))))]
      [`(closure-ref ,v1 ,v2)
        `(closure-ref ,(hoist-lambdas-value v1) ,(hoist-lambdas-value v2))]
      [`(closure-call ,v1 ,vs ...)
        `(closure-call ,(hoist-lambdas-value v1) ,@(map hoist-lambdas-value vs))]
      [`(cletrec ([,alocs (make-closure ,labels ,arity ,vs-list ...)] ...) ,value)
        `(cletrec 
          ,(map
            (lambda (aloc label a vs) 
              `(,aloc 
                (make-closure 
                  ,label
                  ,a
                  ,@(map hoist-lambdas-value vs))))
            alocs labels arity vs-list)
            ,(hoist-lambdas-value value))]
      [`(closure-call ,v ,vs ...)
        `(closure-call ,(hoist-lambdas-value v) ,@(map hoist-lambdas-value vs))]
      [`(call ,v ,vs ...)
        `(call ,(hoist-lambdas-value v) ,@(map hoist-lambdas-value vs))]
      [`(let ([,as ,vs] ...) ,v)
        `(let
          ,(map (lambda (a v) `(,a ,(hoist-lambdas-value v))) as vs) ,(hoist-lambdas-value v))]
      [`(if ,v1 ,v2 ,v3)
        `(if 
          ,(hoist-lambdas-value v1)
          ,(hoist-lambdas-value v2)
          ,(hoist-lambdas-value v3))]
      [`(begin ,es ... ,v)
        `(begin ,@(map hoist-lambdas-effect es) ,(hoist-lambdas-value v))]
      [`(,primop ,vs ...)
        #:when (primop? primop)
        `(,primop ,@(map hoist-lambdas-value vs))]
      [triv
        triv]))

  ;; closure-lang-v9-effect -> hoisted-lang-v9-effect
  (define (hoist-lambdas-effect e)
    (match e
      [`(begin ,es ... ,e)
        `(begin ,@(map hoist-lambdas-effect es) ,(hoist-lambdas-effect e))]
      [`(,primop ,vs ...)
        `(,primop ,@(map hoist-lambdas-value vs))]))

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

  ;; Single letrec
  (check-equal?
    (hoist-lambdas `(module
      (letrec ([L.foo.1 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2))]) 5)))
    `(module (define L.foo.1 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2))) 5))

  ;; Multiple lambdas in single letrec
  ;; Lambda with no arguments
  (check-match
    (hoist-lambdas `(module
      (letrec ([L.foo.1 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2))] [L.bar.2 (lambda () 4)]) 5)))
    `(module
      ,procs ...
      5)
    (equal?
      (list->set procs)
      (list->set
        `((define L.bar.2 (lambda () 4))
          (define L.foo.1 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2)))))))

  ;; Check nested letrec
  (check-match
    (hoist-lambdas `(module
      (letrec ([L.foo.1 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2))])
        (letrec ( [L.bar.2 (lambda () 4)])
          5))))
    `(module
      ,procs ...
      5)
    (equal?
      (list->set procs)
      (list->set 
      `((define L.bar.2 (lambda () 4))
        (define L.foo.1 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2)))))))

  ;; Letrec in lambda
  (check-match
    (hoist-lambdas 
    `(module
      (letrec ([L.foo.1 (lambda () (letrec ( [L.bar.2 (lambda () 4)])
          5))])
        5)))
    `(module
      ,procs ...
      5)
    (equal?
      (list->set procs)
      (list->set 
      `((define L.foo.1 (lambda () 5)) (define L.bar.2 (lambda () 4))))))


  ; Check hoisting in closure-ref, closure-call, call,
  (check-match
    (hoist-lambdas 
    `(module
      (closure-ref 
        (letrec 
          ([L.foo.1 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2))])
          5)
        (closure-call
          (letrec 
            ([L.foo.2 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2))]) 
            (call 
              (letrec 
                ([L.foo.3 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2))]) 
                (cletrec ([x.1 (make-closure L.bar.1 0)])
                  (letrec 
                    ([L.foo.4 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2))])
                    5)))))))))
    `(module
      ,procs ...
      (closure-ref
      5
      (closure-call (call (cletrec ((x.1 (make-closure L.bar.1 0))) 5)))))
    (equal? 
      (list->set procs)
      (list->set 
        `((define L.foo.4 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2)))
          (define L.foo.3 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2)))
          (define L.foo.2 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2)))
          (define L.foo.1 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2)))))))


 ;; Check hoisting in let, if, & begin
  (check-match
    (hoist-lambdas 
    `(module
      (begin
        (begin
          (unsafe-fx+ x.1 x.2)
          (unsafe-fx+
            x.1 
            (letrec 
              ([L.foo.1 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2))])
              5)))
        (if
          (letrec 
            ([L.foo.2 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2))])
            #t)
          (let 
              ([x.1
              (letrec 
                ([L.foo.3 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2))])
                5)])
              (letrec 
                ([L.foo.4 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2))])
                6))
          (letrec 
            ([L.foo.5 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2))])
            5)))))
    `(module
      ,procs ...
      (begin
        (begin (unsafe-fx+ x.1 x.2) (unsafe-fx+ x.1 5))
        (if #t (let ((x.1 5)) 6) 5)))
    (equal?
      (list->set procs)
      (list->set 
        `((define L.foo.5 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2)))
          (define L.foo.4 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2)))
          (define L.foo.3 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2)))
          (define L.foo.2 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2)))
          (define L.foo.1 (lambda (x.1 x.2) (unsafe-fx+ x.1 x.2)))))))
)

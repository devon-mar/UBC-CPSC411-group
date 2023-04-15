#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide implement-closures)

;; Milestone 9 Exercise 12
(define/contract (implement-closures p)
  (-> hoisted-lang-v9 proc-exposed-lang-v9)

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

  (define (implement-closures-p p)
    (match p
      [`(module (define ,labels (lambda (,alocs) ,lambda-values)) ... ,value)
        `(module ,@(map implement-closures-proc labels alocs lambda-values))]))

  ;; label aloc value -> proc
  ;; proc ::= (define label (lambda (aloc ...) value))
  (define (implement-closures-proc label aloc value)
    `(define ,label (lambda ,aloc ,value)))

  ;; hoisted-lang-v9-value -> proc-exposed-lang-v9-value
  (define (implement-closures-value v)
    (match 
      [`(closure-ref ,v1 ,v2)
        `(unsafe-procedure-ref ,v1 ,v2)]
      [`(closure-call ,v ,vs ...)
        `(call (unsafe-procedur-label ,v) ,@vs)]
      [`(cletrec ([,alocs (make-closure ,labels ,arity ,vs)] ...) ,value)
        (define generate-let
          (lambda (value)
            (lambda (body)
              (lambda (assignments)
                `(let ,assignments (begin ,@body ,value))))))
        ;; ([,aloc (make-procedure ,label ,arity ,n)] ...)
        (define let-assignments 
          (for/fold
            ([acc '()])
            ([a alocs]
            [ar arity]
            [v-list vs])
            (cons `[,a (make-procedure ,label ,ar ,(length v-list))] acc)))
        (define let-body 
          (for/fold
            ([outer-acc '()])
            ([a alocs]
             [ar arity]
             [v-list vs])
              (append 
                (for/fold
                  ([inner-acc '()])
                  ([v v-list]
                  [idx (range (length v-list))])
                  (cons `(unsafe-procedure-set! ,a ,idx ,v) inner-acc))
                outer-acc)))
        (((generate-let let-assignments) let-body) value)]
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

  ;; hoisted-lang-v9-effect -> proc-exposed-lang-v9-effect
  (define (implement-closures-effect e)
    (match e
      [`(begin ,es ... ,e)
        (void)]
      [`(,primop ,vs ...)
        (void)]))

  ;; hoisted-lang-v9-triv -> proc-exposed-lang-v9-triv
  (define (implement-closures-triv t)
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

  ;; hoisted-lang-v9-primop -> proc-exposed-lang-v9-primop
  (define (implement-closures-primop p)
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

  (void))


#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide implement-closures)

;; Milestone 9 Exercise 12
;; Implement closures in terms of the procedure data structure
(define/contract (implement-closures p)
  (-> hoisted-lang-v9? proc-exposed-lang-v9?)

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

  ;; hoisted-lang-v9-p proc-exposed-lang-v9-p
  (define (implement-closures-p p)
    (match p
      [`(module (define ,labels (lambda ,alocs ,lambda-values)) ... ,value)
        `(module ,@(map implement-closures-proc labels alocs lambda-values) ,(implement-closures-value value))]))

  ;; label aloc value -> proc
  ;; proc ::= (define label (lambda (aloc ...) value))
  (define (implement-closures-proc label aloc value)
    `(define ,label (lambda ,aloc ,(implement-closures-value value))))

  ;; hoisted-lang-v9-value -> proc-exposed-lang-v9-value
  (define (implement-closures-value v)
    (match v
      [`(closure-ref ,v1 ,v2)
        `(unsafe-procedure-ref ,(implement-closures-value v1) ,(implement-closures-value v2))]
      [`(closure-call ,v ,vs ...)
        `(closure-call (unsafe-procedure-label ,(implement-closures-value v)) ,@(map implement-closures-value vs))]
      [`(cletrec ([,alocs (make-closure ,labels ,arity ,vs ...)] ...) ,value)
        (define let-assignments 
          (for/list
            ([a alocs]
             [ar arity]
             [v-list vs]
             [l labels])
            `[,a (make-procedure ,l ,ar ,(length v-list))]))
        (define let-body 
          (for/fold
            ([acc '()])
            ([a alocs]
             [v-list vs])
              (append 
                (for/list
                  ([v v-list]
                   [idx (range (length v-list))])
                  `(unsafe-procedure-set! ,a ,idx ,(implement-closures-value v)))
                acc)))
        (define let-value (implement-closures-value value))
        (if
          (empty? let-body)
          `(let ,let-assignments ,let-value)
          `(let ,let-assignments (begin ,@let-body ,let-value)))]
      [`(call ,v ,vs ...)
        `(call ,(implement-closures-value v) ,@(map implement-closures-value vs))]
      [`(let ([,as ,vs] ...) ,v)
        `(let ,(map (lambda (a v) `[,a ,(implement-closures-value v)]) as vs) ,(implement-closures-value v))]
      [`(if ,v1 ,v2 ,v3)
        `(if 
          ,(implement-closures-value v1)
          ,(implement-closures-value v2)
          ,(implement-closures-value v3))]
      [`(begin ,es ... ,v)
        `(begin ,@(map implement-closures-effect es) ,(implement-closures-value v))]
      [`(,primop ,vs ...)
        #:when (primop? primop)
        `(,primop ,@(map implement-closures-value vs))]
      [triv
        triv]))

  ;; hoisted-lang-v9-effect -> proc-exposed-lang-v9-effect
  (define (implement-closures-effect e)
    (match e
      [`(begin ,es ... ,e)
        `(begin ,@(map implement-closures-effect es) ,(implement-closures-effect e))]
      [`(,primop ,vs ...)
        `(,primop ,@(map implement-closures-value vs))]))

  ;; Unused
  #;
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

  ;; Unused
  #;
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

  (implement-closures-p p))

(module+ test
  (require rackunit)

  (check-equal?
    (implement-closures
      `(module (closure-ref a.1 b.2)))
    `(module (unsafe-procedure-ref a.1 b.2)))

  ;; Check single make-procedure in cletrec
  (check-match
    (implement-closures
    `(module 
      (cletrec 
        ([x.1 (make-closure L.func.1 1 1 2 3 4)])
        5)))
    `(module
      (let ((x.1 (make-procedure L.func.1 1 4)))
        (begin
          ,set-statements ...
          5)))
    (equal?
      (list->set set-statements)
      (list->set
        `((unsafe-procedure-set! x.1 0 1)
          (unsafe-procedure-set! x.1 1 2)
          (unsafe-procedure-set! x.1 2 3)
          (unsafe-procedure-set! x.1 3 4)))))

  ;; Check multiple make-procedure in cletrec
  (check-match
    (implement-closures
      `(module 
        (cletrec 
            ([x.1 (make-closure L.foo.1 1 1 2 3 4)]
            [x.2 (make-closure L.bar.1 2 8 9 10)])
            5)))
   `(module
      (let ,assignments
        (begin
          ,set-statements ...
          5)))
    (and (equal?
          (list->set set-statements)
          (list->set
            `((unsafe-procedure-set! x.1 0 1)
              (unsafe-procedure-set! x.1 1 2)
              (unsafe-procedure-set! x.1 2 3)
              (unsafe-procedure-set! x.1 3 4)
              (unsafe-procedure-set! x.2 0 8)
              (unsafe-procedure-set! x.2 1 9)
              (unsafe-procedure-set! x.2 2 10))))
          (equal? 
            (list->set assignments)
            (list->set `((x.1 (make-procedure L.foo.1 1 4)) (x.2 (make-procedure L.bar.1 2 3)))))))

  ;; Check cletrec with no values and no arity
  ;; There mustn't be a beign scope
  (check-equal?
    (implement-closures
      `(module 
        (cletrec 
          ([x.1 (make-closure L.func.1 0)])
          5)))
    `(module 
      (let 
        ((x.1 (make-procedure L.func.1 0 0)))
        5)))

  ;; closure-call
  (check-equal?
    (implement-closures
      `(module 
        (cletrec 
          ([x.1 (make-closure L.func.1 1)])
          (closure-call L.func.1 5))))
    `(module
      (let ((x.1 (make-procedure L.func.1 1 0)))
      (call (unsafe-procedure-label L.func.1) 5))))

  ;; closure-call with no args
  (check-equal?
    (implement-closures
      `(module 
        (cletrec 
          ([x.1 (make-closure L.func.1 0)])
          (closure-call L.func.1))))
    `(module
      (let ((x.1 (make-procedure L.func.1 0 0)))
        (call (unsafe-procedure-label L.func.1)))))

  ;; closure-call on cletrec
  (check-equal?
    (implement-closures
      `(module 
        (closure-call L.func.1 
          (cletrec 
            ([x.1 (make-closure L.func.1 1)])
            5))))
    `(module
      (call
        (unsafe-procedure-label L.func.1)
        (let
          ((x.1 (make-procedure L.func.1 1 0)))
          5))))
  
  ;; closure-ref on cletrec
  (check-equal?
    (implement-closures
      `(module 
        (closure-ref 
          (cletrec 
            ([x.1 (make-closure L.func.1 1)])
            5)
          (cletrec 
            ([x.1 (make-closure L.func.1 1)])
            5))))
    `(module
      (unsafe-procedure-ref
        (let ((x.1 (make-procedure L.func.1 1 0))) 5)
        (let ((x.1 (make-procedure L.func.1 1 0))) 5))))


  ;; Check for all value and effect positions
  (check-equal?
    (implement-closures `(module
      (closure-ref
          (cletrec 
            ([x.1 (make-closure L.func.1 1)])
            5)
        (closure-call 
          (cletrec 
            ([x.1 (make-closure L.func.2 1)])
            (call
              (cletrec 
                ([x.1 (make-closure L.func.3 1)])
                (let
                  ([x.1
                    (cletrec 
                      ([x.1 (make-closure L.func.4 1)])
                      5)])
                  (cletrec 
                    ([x.1 (make-closure L.func.5 1)])
                    (begin
                      (begin
                        (unsafe-fx+ 
                          x.1
                          (cletrec 
                            ([x.1 (make-closure L.func.6 1)])
                            5)))
                      (cletrec 
                        ([x.1 (make-closure L.func.7 1)])
                        (if
                          (cletrec 
                            ([x.1 (make-closure L.func.8 1)])
                            #t)
                          (cletrec 
                            ([x.1 (make-closure L.func.9 1)])
                            5)
                          (cletrec 
                            ([x.1 (make-closure L.func.10 1 
                              (cletrec 
                                ([x.1 (make-closure L.func.11 1)])
                                5))])
                            5)))))))))))))
  `(module
    (unsafe-procedure-ref
    (let ((x.1 (make-procedure L.func.1 1 0))) 5)
    (call
      (unsafe-procedure-label
      (let ((x.1 (make-procedure L.func.2 1 0)))
        (call
          (let ((x.1 (make-procedure L.func.3 1 0)))
            (let ((x.1 (let ((x.1 (make-procedure L.func.4 1 0))) 5)))
              (let ((x.1 (make-procedure L.func.5 1 0)))
                (begin
                  (begin
                    (unsafe-fx+
                    x.1
                    (let ((x.1 (make-procedure L.func.6 1 0))) 5)))
                  (let ((x.1 (make-procedure L.func.7 1 0)))
                    (if (let ((x.1 (make-procedure L.func.8 1 0))) #t)
                      (let ((x.1 (make-procedure L.func.9 1 0))) 5)
                      (let ((x.1 (make-procedure L.func.10 1 1)))
                        (begin
                          (unsafe-procedure-set!
                          x.1
                          0
                          (let ((x.1 (make-procedure L.func.11 1 0))) 5))
                          5)))))))))))))))
)

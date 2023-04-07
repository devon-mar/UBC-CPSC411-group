#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7

  (for-syntax racket/syntax))

(provide specify-representation)

;; Milestone 7 Exercise 5
;;
;; Compiles immediate data and primitive operations into their implementations
;; as ptrs and primitive bitwise operations on ptrs.
(define/contract (specify-representation p)
  (-> exprs-unsafe-data-lang-v7? exprs-bits-lang-v7?)

  (define/contract (primop? p)
    (-> any/c boolean?)
    (or (binop? p) (unop? p)))

  (define/contract (binop? b)
    (-> any/c boolean?)
    (and (memq
          b
          '(unsafe-fx*
            unsafe-fx+
            unsafe-fx-
            eq?
            unsafe-fx<
            unsafe-fx<=
            unsafe-fx>
            unsafe-fx>=))
         #t))

  (define/contract (unop? u)
    (-> any/c boolean?)
    (and (memq
          u
          '(fixnum?
            boolean?
            empty?
            void?
            ascii-char?
            error?
            not))
         #t))

  ;; Takes a pred and returns a value that is equal to the true/false ptr.
  ;;
  ;; exprs-bits-lang-v7-pred -> exprs-bits-lang-v7-value
  (define (pred->value p)
    `(if ,p
         ,(current-true-ptr)
         ,(current-false-ptr)))

  ;; Generates (tag-type v) and (untag-type v) functions for the given type
  ;;
  ;; type the name of the type used in the current-*(shift|tag) params.
  ;; v/c: contract for input value
  (define-syntax (make-tag-func stx)
    (syntax-case stx ()
      [(_ type v/c) #`(make-tag-func type v/c values values)]
      [(_ type v/c to-int from-int)
       (with-syntax ([tag (format-id #'type "tag-~a" #'type)]
                     [untag (format-id #'type "untag-~a" #'type)]
                     [shiftp (format-id #'type "current-~a-shift" #'type)]
                     [tagp (format-id #'type "current-~a-tag" #'type)]
                     [maskp (format-id #'type "current-~a-mask" #'type)])

         #'(begin
             (define/contract (tag i)
               (-> v/c int64?)
               (bitwise-ior (arithmetic-shift (to-int i) (shiftp)) (tagp)))
             (define/contract (untag i)
               (-> int64? v/c)
               (from-int (arithmetic-shift i (* -1 (shiftp)))))))]))

  (make-tag-func fixnum int61?)
  (make-tag-func error uint8?)
  (make-tag-func ascii-char ascii-char-literal? char->integer integer->char)

  ;; Generates a function of the form (name?->pred v)
  ;; which returns a exprs-bits-lang-v7-pred.
  (define-syntax (make-unop->pred stx)
    (syntax-case stx ()
      [(_ type)
       (with-syntax ([name (format-id #'type "~a?->pred" #'type)]
                     [tagp (format-id #'type "current-~a-tag" #'type)]
                     [maskp (format-id #'type "current-~a-mask" #'type)])
         ;; generates a exprs-bits-lang-v7-pred to check if v is of type name
         #'(define (name v) `(= (bitwise-and ,v ,(maskp)) ,(tagp))))]))
  (make-unop->pred fixnum)
  (make-unop->pred boolean)
  (make-unop->pred empty)
  (make-unop->pred void)
  (make-unop->pred ascii-char)
  (make-unop->pred error)

  ;; v: exprs-unsafe-data-lang-v7-value
  ;; -> exprs-unsafe-data-lang-v7-proc
  (define/contract (specify-representation-proc label params v)
    (-> label? (listof aloc?) any/c any/c)
    `(define
       ,label
       (lambda ,params
         ,(specify-representation-value/value v))))

  ;; exprs-unsafe-data-lang-v7-p exprs-bits-lang-v7-p
  (define (specify-representation-p p)
    (match p
      [`(module (define ,labels (lambda (,alocs ...) ,values)) ... ,value)
       `(module
            ,@(map specify-representation-proc labels alocs values)
          ,(specify-representation-value/value value))]))

  ;; exprs-unsafe-data-lang-v7-p exprs-bits-lang-v7-value
  (define (specify-representation-value/value v)
    (match v
      ;; modified template - removed tail v
      [`(call ,vs ...)
       `(call ,@(map specify-representation-value/value vs))]
      [`(let ([,as ,vs] ...) ,v)
       `(let
            ,(map (lambda (a v) `[,a ,(specify-representation-value/value v)]) as vs)
          ,(specify-representation-value/value v))]
      [`(if ,v1 ,v2 ,v3)
       `(if ,(specify-representation-value/pred v1)
            ,(specify-representation-value/value v2)
            ,(specify-representation-value/value v3))]
      [`(,primop ,vs ...)
       #:when (primop? primop)
       (specify-representation-primop/value primop vs)]
      [_ (specify-representation-triv/value v)]))

  ;; exprs-unsafe-data-lang-v7-triv exprs-bits-lang-v7-triv
  (define (specify-representation-triv/value t)
    (match t
      [(? label?)
       t]
      [(? aloc?)
       t]
      [(? int61?)
       (tag-fixnum t)]
      [#t
       (current-true-ptr)]
      [#f
       (current-false-ptr)]
      ['empty
       (current-empty-ptr)]
      ['(void)
       (current-void-ptr)]
      [`(error ,uint8)
       (tag-error uint8)]
      [(? ascii-char-literal?)
       (tag-ascii-char t)]))

  ;; exprs-unsafe-data-lang-v7-primop (listof exprs-unsafe-data-lang-v7-value) -> exprs-bits-lang-v7-value
  (define (specify-representation-primop/value p vs)
    (match p
      [(? binop?)
       (specify-representation-binop/value p vs)]
      [(? unop?)
       (specify-representation-unop/value p vs)]))

  ;; exprs-unsafe-data-lang-v7-unop (listof exprs-unsafe-data-lang-v7-value) -> exprs-bits-lang-v7-value
  (define (specify-representation-unop/value u vs)
    (match u
      ['not
       `(if ,(specify-representation-value/pred (car vs))
            ,(current-false-ptr)
            ,(current-true-ptr))]
      [_ (pred->value (specify-representation-unop/pred u vs))]))

  ;; exprs-unsafe-data-lang-v7-binop (listof exprs-unsafe-data-lang-v7-value) -> exprs-bits-lang-v7-value
  (define/contract (specify-representation-binop/value b vs)
    (-> symbol? (listof any/c) any/c)
    ;; It is very important that this match doesn't contain cases
    ;; that the /pred version matches!
    (match b
      ['unsafe-fx*
       (match vs
         [`(,i ,v)
          #:when (int64? i)
          `(* ,i ,(specify-representation-value/value v))]
         [`(,v ,i)
          #:when (int64? i)
          `(* ,(specify-representation-value/value v) ,i)]
         [`(,v1 ,v2)
          `(* ,v1 (arithmetic-shift-right ,(specify-representation-value/value v2) ,(current-fixnum-shift)))])]
      ['unsafe-fx+
       `(+ ,@(map specify-representation-value/value vs))]
      ['unsafe-fx-
       `(- ,@(map specify-representation-value/value vs))]
      ;; modfied template - squashed cases - the rest all need to be converted
      ;; to (if (relop ,vs) true false)
      [_ (pred->value (specify-representation-binop/pred b vs))]))

  ;; The following functions' return value may only be used in pred position of
  ;; the target language.
  ;; exprs-unsafe-data-lang-v7-p exprs-bits-lang-v7-pred
  (define (specify-representation-value/pred v)
    (match v
      [`(call ,vs ...)
       `(!= (call ,@(map specify-representation-value/value vs)) ,(current-false-ptr))]
      [`(let ([,as ,vs] ...) ,v)
       `(let
            ,(map (lambda (a v) `[,a ,(specify-representation-value/value v)]) as vs)
          ,(specify-representation-value/pred v))]
      [`(if ,v1 ,v2 ,v3)
       `(if ,(specify-representation-value/pred v1)
            ,(specify-representation-value/pred v2)
            ,(specify-representation-value/pred v3))]
      [`(,primop ,vs ...)
       #:when (primop? primop)
       (specify-representation-primop/pred primop vs)]
      [_ (specify-representation-triv/pred v)]))

  ;; exprs-unsafe-data-lang-v7-triv exprs-bits-lang-v7-pred
  (define (specify-representation-triv/pred t)
    ;; Modified template - we return #f is t==#f, otherwise #t
    (match t
      [#f `(false)]
      [_ `(true)]))

  ;; exprs-unsafe-data-lang-v7-primop (listof exprs-unsafe-data-lang-v7-value) -> exprs-bits-lang-v7-pred
  (define (specify-representation-primop/pred p vs)
    (match p
      [(? binop?)
       (specify-representation-binop/pred p vs)]
      [(? unop?)
       (specify-representation-unop/pred p vs)]))

  ;; exprs-unsafe-data-lang-v7-binop (listof exprs-unsafe-data-lang-v7-value) -> exprs-bits-lang-v7-pred
  (define/contract (specify-representation-binop/pred b vs)
    (-> symbol? (listof any/c) any/c)
    (match b
      ;; It is very important that this match doesn't contain cases
      ;; that the /value version matches!
      ['eq? `(= ,@(map specify-representation-value/value vs))]
      ['unsafe-fx< `(< ,@(map specify-representation-value/value vs))]
      ['unsafe-fx<= `(<= ,@(map specify-representation-value/value vs))]
      ['unsafe-fx> `(> ,@(map specify-representation-value/value vs))]
      ['unsafe-fx>= `(>= ,@(map specify-representation-value/value vs))]
      ;; Modified template - the rest all operate on and return fixnums (hopefully)
      ;; TODO
      ;; The pass before this pass is `implement-safe-primops` and should make sure
      ;; that all arguments to unsafe-fx[+-*] so this should always be true ...
      #;
      [_ `(true)]
      ;; but just in case ...
      [_ `(!= ,(specify-representation-binop/value b vs), (current-false-ptr))]))

  ;; exprs-unsafe-data-lang-v7-unop (listof exprs-unsafe-data-lang-v7-value) -> exprs-bits-lang-v7-pred
  (define (specify-representation-unop/pred u vs)
    (define v (car vs))
    (match u
      ['not
       `(not ,(specify-representation-value/pred v))]
      ;; modified template - added nested match since the rest need
      ;; (specify-representation-value/value v)
      [_
       ((match u
          ['fixnum? fixnum?->pred]
          ['boolean? boolean?->pred]
          ['empty? empty?->pred]
          ['void? void?->pred]
          ['ascii-char? ascii-char?->pred]
          ['error? error?->pred])
        (specify-representation-value/value v))]))

  (specify-representation-p p))

(module+ test
  (require rackunit)

  (define-check (check-42 p)
    (define v (interp-exprs-bits-lang-v7 (specify-representation p)))
    (check-equal? (bitwise-and v (current-fixnum-mask)) (current-fixnum-tag))
    (check-equal? (arithmetic-shift v (* -1 (current-fixnum-shift))) 42))

  (define-check (check-eval-true p)
    (check-equal?
     (interp-exprs-bits-lang-v7 (specify-representation p))
     (current-true-ptr)
     (format "expected true: ~a" p)))

  (define-check (check-eval-false p)
    (check-equal?
     (interp-exprs-bits-lang-v7 (specify-representation p))
     (current-false-ptr)
     (format "expected false ~a" p)))

  (define-syntax-rule (triv-test i o)
    (check-equal?
     (specify-representation `(module ,i))
     `(module ,o)))
  ;; triv/label
  (triv-test 'L.a.1 'L.a.1)

  ;; triv/aloc
  (triv-test 'x.1 'x.1)
  ;; triv/fixnum
  (triv-test 1 #b1000)
  (triv-test 0 #b000)
  (triv-test 42 #b101010000)

  ;; triv/#t
  (triv-test '#t (current-true-ptr))
  ;; triv/#f
  (triv-test '#f (current-false-ptr))
  ;; triv/empty
  (triv-test 'empty (current-empty-ptr))
  ;; triv/(void)
  (triv-test '(void) (current-void-ptr))
  ;; triv/(error uint8)
  (triv-test '(error 0) #b000111110)
  (triv-test '(error 1) #b100111110)
  (triv-test '(error 42) #b10101000111110)
  ;; triv/ascii-char-literal
  (triv-test '#\a #b110000100101110)
  (triv-test '#\b #b110001000101110)

  (define-syntax-rule (binop-test b x y)
    (check-42 '(module (b x y))))

  ;; binop/unsafe-fx+
  (binop-test unsafe-fx+ 40 2)
  ;; binop/unsafe-fx-
  (binop-test unsafe-fx- 50 8)
  ;; binop/unsafe-fx*
  ;; both are fixnums
  (binop-test unsafe-fx* 21 2)

  ;; lhs is fixnum
  (check-42
   '(module
      (let ([x.1 21])
        (unsafe-fx* 2 x.1))))
  ;; rhs is fixnum
  (check-42
   '(module
      (let ([x.1 2])
        (unsafe-fx* x.1 21))))
  ;; neither are fixnums
  (check-42
   '(module
      (let ([x.1 2]
            [y.1 21])
        (unsafe-fx* x.1 y.1))))

  (define-syntax-rule (relop-test b tx ty fx fy)
    (test-case
     (format "relop ~a" 'b)
     (check-eval-true '(module (b tx ty)))
     (check-eval-false '(module (b fx fy)))))

  ;; binop/eq?
  (relop-test eq? 42 42 1 2)
  ;; binop/unsafe-fx<
  (relop-test unsafe-fx< 10 20 20 10)
  ;; binop/unsafe-fx<=
  (relop-test unsafe-fx<= 10 10 20 10)
  (relop-test unsafe-fx<= 8 10 20 10)
  ;; binop/unsafe-fx>
  (relop-test unsafe-fx> 20 10 10 10)
  (relop-test unsafe-fx> 20 10 10 20)
  ;; binop/unsafe-fx>=
  (relop-test unsafe-fx>= 20 10 10 11)
  (relop-test unsafe-fx>= 20 20 10 11)

  (define-syntax-rule (unop-test unop t fs ...)
    (begin
      (check-equal?
       (interp-exprs-bits-lang-v7
        (specify-representation '(module (unop t))))
       (current-true-ptr)
       (format "expected true: (~a ~a)" 'unop 't))
      (check-equal?
       (interp-exprs-bits-lang-v7
        (specify-representation '(module (unop fs))))
       (current-false-ptr)
       (format "expected false: (~a ~a)" 'unop 'fs)) ...))

  ;; unop/fixnum?
  (unop-test fixnum? 1 #t #f empty (void) #\a (error 1))
  (unop-test fixnum? 42)

  ;; unop/boolean?
  (unop-test boolean? #t 1 empty (void) #\a (error 1))
  (unop-test boolean? #f)

  ;; unop/empty?
  (unop-test empty? empty 1 #t #f (void) #\a (error 1))
  ;; unop/void?
  (unop-test void? (void) 1 #t #f empty #\a (error 1))
  ;; unop/ascii-char?
  (unop-test ascii-char? #\a 1 #t #f empty (void) (error 1))
  (unop-test ascii-char? #\b 1 #t #f empty (void) (error 1))
  ;; unop/error?
  (unop-test error? (error 1) 1 #t #f empty (void) #\a)
  (unop-test error? (error 42) 1 #t #f empty (void) #\a)
  ;; unop/not
  (check-eval-false '(module (not (fixnum? 1))))
  (check-eval-true '(module (not (not (fixnum? 1)))))
  (check-eval-true '(module (not (fixnum? empty))))

  ;; value/(if value value value)
  (check-42
    '(module (if 10 42 0)))
  (check-42
    '(module (if #f 0 42)))
  (check-42
    '(module
       (if (eq? 10 10)
         42
         2)))
  (check-42
    '(module
       (if (not (eq? 10 10))
         2
         42)))
  (check-eval-false
    '(module (not (eq? 10 10))))
  (check-eval-true
    '(module (not (not (eq? 10 10)))))

  ;; value/(call value value ...)
  (check-42
    '(module
       (define L.add.1 (lambda (x.1 y.1) (unsafe-fx+ x.1 y.1)))
       (call L.add.1 40 2)))
  (check-42
    '(module
       (define L.ft.1 (lambda () 42))
       (call L.ft.1)))

  ;; value/(let ([aloc value] ...) value)
  (check-42
   '(module
        (let([x.1 8])
          (let ([a.1 (unsafe-fx* x.1 3)]
                [b.1 18])
            (unsafe-fx+ a.1 b.1)))))

  ;; nested ifs
  (check-42
    '(module (if (if (eq? 10 20) (eq? 10 11) (eq? 1 1)) 42 0)))

  ;; let in pred position
  (check-42
    '(module
       (if (let ([x.1 10]) (eq? x.1 10))
         42
         0)))

  ;; call in pred position
  (check-42
    '(module
       (define L.myeq.1 (lambda (x.1 y.1) (eq? x.1 y.1)))
       (if (call L.myeq.1 10 20)
         0
         42)))
  )

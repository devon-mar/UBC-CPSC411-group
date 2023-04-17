#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  cpsc411/langs/v8

  (for-syntax racket/syntax))

(provide specify-representation)

;; Milestone 7 Exercise 5
;; Milestone 8 Exercise 4
;; Milestone 9 Exercise 13
;;
;; Compiles immediate data and primitive operations into their implementations
;; as ptrs and primitive bitwise operations on ptrs.
;;
;; Implementation details
;; - For some proc-exposed-lang-v9 non-terminals we have multiple corresponding functions based on the
;;   template depending on where the return value is to be used in the output exprs-bits-lang-v8.
;;   Such functions will have a /(pred|value|effect) in their name. This allows us to implement certain
;;   optimisations (such as on not) (as recommended in OH) since we know where our output is going to be used.
;;
;; - If any template modifications ever need to be undone, the original template can be found in "m9/templates.rkt"
(define/contract (specify-representation p)
  (-> proc-exposed-lang-v9? exprs-bits-lang-v8?)

  ;; for convenience...
  (define pair-car-offset (* -1 (current-pair-tag)))
  (define pair-cdr-offset (- (current-word-size-bytes) (current-pair-tag)))
  (define vec-len-offset (* -1 (current-vector-tag)))
  (define vec-base-offset (+ (current-word-size-bytes) vec-len-offset)) ;; offset of the 0th element
  (define procedure-label-offset (* -1 (current-procedure-tag)))
  (define procedure-arity-offset (- (current-word-size-bytes) (current-procedure-tag)))
  (define procedure-env-base-offset (- (* 2 (current-word-size-bytes)) (current-procedure-tag)))

  ;; Returns a value that evaluates to a PTR with the given tag
  ;; that points to a location good for n bytes.
  ;;
  ;; n: proc-exposed-lang-v9-value
  ;; tag: int64?
  ;; f: (-> aloc (listof exprs-bits-lang-v8-effect))
  ;; -> exprs-bits-lang-v8-value (PTR)
  (define/contract (alloc/tag n tag f)
    (-> any/c int64? (-> aloc? (listof any/c)) any/c)
    (define tmp (fresh))
   `(let ([,tmp (+ (alloc ,n) ,tag)])
      (begin
        ,@(f tmp)
        ,tmp)))

  ;; Returns an offset for use with mset! or mref for the ith element of a vector.
  ;;
  ;; proc-exposed-lang-v9-value -> exprs-bits-lang-v8-value
  (define (vec-idx->vec-offset idx)
    (if (int64? idx)
      (+ (* (current-word-size-bytes) idx) vec-base-offset)
      `(+ (* ,(untag-fixnum (specify-representation-value/value idx)) ,(current-word-size-bytes)) ,vec-base-offset)))

  ;; Returns an offset for the ith var in a procedure's environment.
  ;;
  ;; proc-exposed-lang-v9-value -> exprs-bits-lang-v8-value
  (define (proc-env-idx->offset i)
    (if (int64? i)
     (+ procedure-env-base-offset (* i (current-word-size-bytes)))
     `(+ (* ,(untag-fixnum (specify-representation-value/value i)) ,(current-word-size-bytes))
         ,procedure-env-base-offset)))

  ;; Returns #t if p is a proc-exposed-lang-v9-primop, otherwise returns #f.
  (define/contract (primop? p)
    (-> any/c boolean?)
    (and
      (memq p
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
              make-procedure
              unsafe-procedure-arity
              unsafe-procedure-label
              unsafe-procedure-ref
              unsafe-procedure-set!))
      #t))

  ;; Takes a pred and returns a value that returns #t or #f based on p.
  ;;
  ;; proc-exposed-lang-v9-pred -> exprs-bits-lang-v8-value
  (define (pred->value p)
    `(if ,p
         ,(current-true-ptr)
         ,(current-false-ptr)))

  ;; Usage:
  ;; (make-tag-func type v/c)
  ;; (make-tag-func type v/c to-int-proc from-int-proc)
  ;;
  ;; type: (unquotted) name of the type. Must have a corresponding current-type-(shift|tag) param.
  ;; v/c: A contract? for the input value to the static-tag function.
  ;; to-int-proc: A procedure that takes a v/c and turns it into an int64? representation.
  ;; from-int-proc: A procedure that takes an int64? and turns it into a v/c.
  ;;
  ;; Generates (static-tag-type v) and (untag-type v) functions for the given type.
  ;; (static-tag-type v) will statically tag v.
  ;; (untag-type exprs-bits-lang-v8-value) will return a exprs-bits-lang-v8-value that will tag v at run time.
  ;;
  ;; Examples:
  ;; (make-tag-func fixnum int61?) -> (begin (define (static-tag-fixnum v) ...) (define (untag-fixnum v) ...)
  (define-syntax (make-tag-func stx)
    (syntax-case stx ()
      [(_ type v/c) #`(make-tag-func type v/c values values)]
      [(_ type v/c to-int from-int)
       (with-syntax ([tag (format-id #'type "static-tag-~a" #'type)]
                     [untag (format-id #'type "untag-~a" #'type)]
                     [shiftp (format-id #'type "current-~a-shift" #'type)]
                     [tagp (format-id #'type "current-~a-tag" #'type)]
                     [maskp (format-id #'type "current-~a-mask" #'type)])

         #'(begin
             (define/contract (tag i)
               (-> v/c int64?)
               (bitwise-ior (arithmetic-shift (to-int i) (shiftp)) (tagp)))
             (define (untag i)
               `(arithmetic-shift-right ,i ,(shiftp)))))]))

  (make-tag-func fixnum int61?)
  (make-tag-func error uint8?)
  (make-tag-func ascii-char ascii-char-literal? char->integer integer->char)

  ;; Usage:
  ;; (make-primop-pred type v)
  ;; type: A PTR type (must have a current-type-(tag|mask) parameter)
  ;; v: the value to check.
  ;;
  ;; Returns a exprs-bits-lang-v8-pred that checks if v is of type type.
  ;;
  ;; Examples:
  ;; (make-primop-pred fixnum 123) -> `(= (bitwise-and ,123 7) 0)
  ;; (make-primop-pred error x) -> `(= (bitwise-and ,x 255) 62)
  (define-syntax (make-primop-pred stx)
    (syntax-case stx ()
      [(_ type v)
       (with-syntax ([tagp (format-id #'type "current-~a-tag" #'type)]
                     [maskp (format-id #'type "current-~a-mask" #'type)])
         #'`(= (bitwise-and ,v ,(maskp)) ,(tagp)))]))

  ;; v: proc-exposed-lang-v9-value
  ;; -> proc-exposed-lang-v9-proc
  (define/contract (specify-representation-proc label params v)
    (-> label? (listof aloc?) any/c any/c)
    `(define
       ,label
       (lambda ,params
         ,(specify-representation-value/value v))))

  ;; proc-exposed-lang-v9-p exprs-bits-lang-v8-p
  (define (specify-representation-p p)
    (match p
      [`(module (define ,labels (lambda (,alocs ...) ,values)) ... ,value)
       `(module
            ,@(map specify-representation-proc labels alocs values)
          ,(specify-representation-value/value value))]))

  ;; proc-exposed-lang-v9-p exprs-bits-lang-v8-value
  (define (specify-representation-value/value v)
    (match v
      ;; modified template - removed tail value
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
      [`(begin ,es ... ,v)
        `(begin
           ,@(map specify-representation-effect/effect es)
           ,(specify-representation-value/value v))]
      [`(,primop ,vs ...)
        #:when (primop? primop)
        (specify-representation-primop/value primop vs)]
      [_ (specify-representation-triv/value v)]))

  ;; proc-exposed-lang-v9 -> exprs-bits-lang-v8-effect
  (define (specify-representation-effect/effect e)
    (match e
      ;; Modified template - Removed 'tail' effect since we assume valid input
      [`(begin ,es ...)
        `(begin ,@(map specify-representation-effect/effect es))]
      [`(,primop ,vs ...)
        #:when (primop? primop)
        (specify-representation-primop/effect primop vs)]))

  ;; "only imperative primops (only unsafe-vector-set! so far) can be directly called in effect context"
  ;;
  ;; proc-exposed-lang-v9-primop (listof proc-exposed-lang-v9-value) -> exprs-bits-lang-v8-effect
  (define (specify-representation-primop/effect p vs)
    (match (cons p vs)
      [`(unsafe-vector-set! ,vec ,idx ,val)
        `(mset!
           ,(specify-representation-value/value vec)
           ,(vec-idx->vec-offset idx)
           ,(specify-representation-value/value val))]
      [`(unsafe-procedure-set! ,proc ,idx ,val)
        `(mset!
           ,(specify-representation-value/value proc)
           ,(proc-env-idx->offset idx)
           ,(specify-representation-value/value val))]))

  ;; proc-exposed-lang-v9-triv exprs-bits-lang-v8-triv
  (define (specify-representation-triv/value t)
    (match t
      [(? label?)
       t]
      [(? aloc?)
       t]
      [(? int61?)
       (static-tag-fixnum t)]
      [#t
       (current-true-ptr)]
      [#f
       (current-false-ptr)]
      ['empty
       (current-empty-ptr)]
      ['(void)
       (current-void-ptr)]
      [`(error ,uint8)
       (static-tag-error uint8)]
      [(? ascii-char-literal?)
       (static-tag-ascii-char t)]))

  ;; "Only primops that produce values can appear in value context"
  ;;
  ;; proc-exposed-lang-v9-primop (listof proc-exposed-lang-v9-value) -> exprs-bits-lang-v8-value
  (define (specify-representation-primop/value p vs)
    (match (cons p vs)
      [`(unsafe-fx* ,vs ...)
       (match vs
         [`(,i ,v)
          #:when (int64? i)
          `(* ,i ,(specify-representation-value/value v))]
         [`(,v ,i)
          #:when (int64? i)
          `(* ,(specify-representation-value/value v) ,i)]
         [`(,v1 ,v2)
          `(* ,v1 (arithmetic-shift-right ,(specify-representation-value/value v2) ,(current-fixnum-shift)))])]
      [`(unsafe-fx+ ,vs ...)
       `(+ ,@(map specify-representation-value/value vs))]
      [`(unsafe-fx- ,vs ...)
       `(- ,@(map specify-representation-value/value vs))]
      [`(not ,v)
       `(if ,(specify-representation-value/pred v)
            ,(current-false-ptr)
            ,(current-true-ptr))]
      [`(cons ,x ,y)
        (alloc/tag
          (current-pair-size)
          (current-pair-tag)
          (lambda (aloc)
            `((mset!
                ,aloc
                ,pair-car-offset
                ,(specify-representation-value/value x))
              (mset!
                ,aloc
                ,pair-cdr-offset
                ,(specify-representation-value/value y)))))]
      [`(unsafe-car ,p)
       `(mref ,(specify-representation-value/value p) ,pair-car-offset)]
      [`(unsafe-cdr ,p)
       `(mref ,(specify-representation-value/value p) ,pair-cdr-offset)]
      [`(unsafe-make-vector ,len)
        (define new-len (specify-representation-value/value len))
        (if (int64? len)
          (alloc/tag
            (* (current-word-size-bytes) (add1 len))
            (current-vector-tag)
            (lambda (a) `((mset! ,a ,vec-len-offset ,new-len))))
          (alloc/tag
            `(* (+ 1 ,(untag-fixnum new-len)) ,(current-word-size-bytes))
            (current-vector-tag)
            (lambda (a) `((mset! ,a ,vec-len-offset ,new-len)))))]
      [`(unsafe-vector-length ,v)
       `(mref ,(specify-representation-value/value v) ,vec-len-offset)]
      [`(unsafe-vector-ref ,vec ,idx)
       `(mref ,(specify-representation-value/value vec) ,(vec-idx->vec-offset idx))]
      ;; modified template - removed unsafe-vector-set! since it should never
      ;; appear in value context
      [`(unsafe-procedure-label ,proc)
       `(mref ,(specify-representation-value/value proc) ,procedure-label-offset)]
      [`(unsafe-procedure-arity ,proc)
       `(mref ,(specify-representation-value/value proc) ,procedure-arity-offset)]
      [`(unsafe-procedure-ref ,proc ,idx)
       `(mref ,(specify-representation-value/value proc)
              ,(proc-env-idx->offset idx))]
      ;; "When implementing make-procedure, we assume the size of the environment
      ;; is a fixnum constant, since this is guaranteed by how our compiler generates make-procedure"
      [`(make-procedure ,label ,arity ,size)
        (alloc/tag
          ;; we need:
          ;; 1 word for the label
          ;; 1 word for arity
          ;; size words for the env
          (* (current-word-size-bytes) (+ 2 size))
          (current-procedure-tag)
          (lambda (a)
            `((mset! ,a ,procedure-label-offset ,(specify-representation-value/value label))
              (mset! ,a ,procedure-arity-offset ,(specify-representation-value/value arity)))))]
      ;; modfied template - squashed cases - the rest all need to be converted
      ;; to something of the form (if (!= (relop ,@vs) #f) #t #f)
      ;;
      ;; 'eq?
      ;; 'unsafe-fx<
      ;; 'unsafe-fx<=
      ;; 'unsafe-fx>
      ;; 'unsafe-fx>=
      ;; 'fixnum?
      ;; 'boolean?
      ;; 'empty?
      ;; 'void?
      ;; 'ascii-char?
      ;; 'error?
      ;; 'pair?
      ;; 'vector?
      ;; procedure?
      [_ (pred->value (specify-representation-primop/pred p vs))]))

  ;; Takes an exprs-bits-lang-v8-value and transorms it into an
  ;; exprs-bits-lang-v8-pred by checking if the value is != #f.
  ;;
  ;; exprs-bits-lang-v8-value -> exprs-bits-lang-v8-pred
  (define (value->pred v)
    `(!= ,v ,(current-false-ptr)))

  ;; The following functions' return value may only be used in pred position of
  ;; the target language.
  ;; proc-exposed-lang-v9-p exprs-bits-lang-v8-pred
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

  ;; proc-exposed-lang-v9-primop (listof proc-exposed-lang-v9-value) -> exprs-bits-lang-v8-value
  (define (specify-representation-primop/pred p vs)
    (define v (car vs))
    (match p
      ['eq? `(= ,@(map specify-representation-value/value vs))]
      ['unsafe-fx< `(< ,@(map specify-representation-value/value vs))]
      ['unsafe-fx<= `(<= ,@(map specify-representation-value/value vs))]
      ['unsafe-fx> `(> ,@(map specify-representation-value/value vs))]
      ['unsafe-fx>= `(>= ,@(map specify-representation-value/value vs))]
      ['not
       `(not ,(specify-representation-value/pred v))]
      ;; modified template - squashed some cases that all do the same thing
      ;;
      ;; must be careful to make sure that specify-representation-primop/value
      ;; has an explicit match for the below symbols
      [(or 'cons 'unsafe-car 'unsafe-cdr 'unsafe-make-vector 'unsafe-vector-length
           'unsafe-vector-ref 'unsafe-fx* 'unsafe-fx+ 'unsafe-fx- 'make-procedure
           'unsafe-procedure-arity 'unsafe-procedure-label 'unsafe-procedure-ref)
       (value->pred (specify-representation-primop/value p vs))]
      ;; note: we intentionally skip unsafe-procedure-set!
      ;; since it should never show up in pred position
      ;;
      ;; modified template - added nested match since the rest need to be of the form
      ;; (pred (specify-representation-value/value v))
      [_
        (define v-new (specify-representation-value/value v))
        (match p
          ['fixnum? (make-primop-pred fixnum v-new)]
          ['boolean? (make-primop-pred boolean v-new)]
          ['empty? (make-primop-pred empty v-new)]
          ['void? (make-primop-pred void v-new)]
          ['ascii-char? (make-primop-pred ascii-char v-new)]
          ['error? (make-primop-pred error v-new)]
          ['pair? (make-primop-pred pair v-new)]
          ['vector? (make-primop-pred vector v-new)]
          ['procedure? (make-primop-pred procedure v-new)])]))

  ;; proc-exposed-lang-v9-triv exprs-bits-lang-v8-pred
  (define (specify-representation-triv/pred t)
    ;; Modified template - we return #f if t==#f, otherwise #t
    (match t
      [#f `(false)]
      [(? aloc?) `(!= ,t ,(current-false-ptr))]
      ;; modfied template - removed the other literal cases
      ;; because they're all != #f
      [_ `(true)]))

  (specify-representation-p p))

(module+ test
  (require rackunit)

  (define-check (check-42 p)
    (define v (interp-exprs-bits-lang-v8 (specify-representation p)))
    (check-equal? (bitwise-and v (current-fixnum-mask)) (current-fixnum-tag))
    (check-equal? (arithmetic-shift v (* -1 (current-fixnum-shift))) 42))

  (define-check (check-eval-true p)
    (check-equal?
     (interp-exprs-bits-lang-v8 (specify-representation p))
     (current-true-ptr)
     (format "expected true: ~a" p)))

  (define-check (check-eval-false p)
    (check-equal?
     (interp-exprs-bits-lang-v8 (specify-representation p))
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
       (interp-exprs-bits-lang-v8
        (specify-representation '(module (unop t))))
       (current-true-ptr)
       (format "expected true: (~a ~a)" 'unop 't))
      (check-equal?
       (interp-exprs-bits-lang-v8
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

  ;; should work on the output of the reference implementation's
  ;; safe primops


  (define safe-primops
    '([*
       (lambda (tmp.14 tmp.15)
         (if (fixnum? tmp.15)
           (if (fixnum? tmp.14) (unsafe-fx* tmp.14 tmp.15) (error 1))
           (error 1)))]

      [+
       (lambda (tmp.16 tmp.17)
         (if (fixnum? tmp.17)
           (if (fixnum? tmp.16) (unsafe-fx+ tmp.16 tmp.17) (error 2))
           (error 2)))]

      [-
       (lambda (tmp.18 tmp.19)
         (if (fixnum? tmp.19)
           (if (fixnum? tmp.18) (unsafe-fx- tmp.18 tmp.19) (error 3))
           (error 3)))]

      [<
       (lambda (tmp.20 tmp.21)
         (if (fixnum? tmp.21)
           (if (fixnum? tmp.20) (unsafe-fx< tmp.20 tmp.21) (error 4))
           (error 4)))]

      [<=
       (lambda (tmp.22 tmp.23)
         (if (fixnum? tmp.23)
           (if (fixnum? tmp.22) (unsafe-fx<= tmp.22 tmp.23) (error 5))
           (error 5)))]

      [>
       (lambda (tmp.24 tmp.25)
         (if (fixnum? tmp.25)
           (if (fixnum? tmp.24) (unsafe-fx> tmp.24 tmp.25) (error 6))
           (error 6)))]

      [>=
       (lambda (tmp.26 tmp.27)
         (if (fixnum? tmp.27)
           (if (fixnum? tmp.26) (unsafe-fx>= tmp.26 tmp.27) (error 7))
           (error 7)))]
      [eq? (lambda (tmp.48 tmp.49) (eq? tmp.48 tmp.49))]
      [fixnum? (lambda (tmp.37) (fixnum? tmp.37))]
      [boolean? (lambda (tmp.38) (boolean? tmp.38))]
      [empty? (lambda (tmp.39) (empty? tmp.39))]
      [void? (lambda (tmp.40) (void? tmp.40))]
      [ascii-char? (lambda (tmp.41) (ascii-char? tmp.41))]
      [error? (lambda (tmp.42) (error? tmp.42))]
      [not (lambda (tmp.45) (not tmp.45))]
      [pair? (lambda (tmp.43) (pair? tmp.43))]
      [vector? (lambda (tmp.44) (vector? tmp.44))]
      [procedure? (lambda (tmp.50) (procedure? tmp.50))]))

  (define-syntax-rule (safe-primop-module primop args ...)
    `(module
       (define ,(string->symbol (format "L.~a.1" 'primop)) ,@(dict-ref safe-primops 'primop))
       (call ,(string->symbol (format "L.~a.1" 'primop)) args ...)))

  ;; *
  (check-42 (safe-primop-module * 21 2))

  ;; +
  (check-42 (safe-primop-module + 40 2))

  ;; -
  (check-42 (safe-primop-module - 50 8))

  ;; <
  (check-eval-true (safe-primop-module < 1 2))
  (check-eval-false (safe-primop-module < 1 1))
  (check-eval-false (safe-primop-module < 2 1))

  ;; <=
  (check-eval-true (safe-primop-module <= 1 2))
  (check-eval-true (safe-primop-module <= 1 1))
  (check-eval-false (safe-primop-module <= 2 1))

  ;; >
  (check-eval-true (safe-primop-module > 3 2))
  (check-eval-false (safe-primop-module > 1 1))
  (check-eval-false (safe-primop-module > 0 1))

  ;; >=
  (check-eval-true (safe-primop-module >= 4 2))
  (check-eval-true (safe-primop-module >= 1 1))
  (check-eval-false (safe-primop-module >= 0 1))

  ;; eq?
  (check-eval-true (safe-primop-module eq? 0 0))
  (check-eval-false (safe-primop-module eq? 1 0))

  ;; fixnum?
  (check-eval-true (safe-primop-module fixnum? 0))
  (check-eval-false (safe-primop-module fixnum? #\a))

  ;; boolean?
  (check-eval-true (safe-primop-module boolean? #t))
  (check-eval-true (safe-primop-module boolean? #f))
  (check-eval-false (safe-primop-module boolean? #\a))

  ;; empty?
  (check-eval-true (safe-primop-module empty? empty))
  (check-eval-false (safe-primop-module empty? #\a))

  ;; void?
  (check-eval-true (safe-primop-module void? (void)))
  (check-eval-false (safe-primop-module void? #\a))

  ;; ascii-char?
  (check-eval-true (safe-primop-module ascii-char? #\a))
  (check-eval-false (safe-primop-module ascii-char? (void)))

  ;; error?
  (check-eval-true (safe-primop-module error? (error 123)))
  (check-eval-false (safe-primop-module error? (void)))

  ;; not
  (check-eval-true (safe-primop-module not #f))
  (check-eval-false (safe-primop-module not #t))

  ;; pair?
  (check-eval-true (safe-primop-module pair? (cons 1 2)))
  (check-eval-false (safe-primop-module pair? (void)))

  ;; vector?
  ;; make-vector
  (check-eval-false (safe-primop-module vector? (void)))
  (check-eval-true
    '(module
       (define L.vector?.6 (lambda (tmp.44) (vector? tmp.44)))
       (define L.vector-init-loop.4
         (lambda (len.3 i.5 vec.4)
           (if (eq? len.3 i.5)
             vec.4
             (begin
               (unsafe-vector-set! vec.4 i.5 0)
               (call L.vector-init-loop.4 len.3 (unsafe-fx+ i.5 1) vec.4)))))
       (define L.make-init-vector.1
         (lambda (tmp.1)
           (if (unsafe-fx>= tmp.1 0)
             (let ((tmp.2 (unsafe-make-vector tmp.1)))
               (call L.vector-init-loop.4 tmp.1 0 tmp.2))
             (error 12))))
       (define L.make-vector.5
         (lambda (tmp.28)
           (if (fixnum? tmp.28) (call L.make-init-vector.1 tmp.28) (error 8))))
       (call L.vector?.6 (call L.make-vector.5 1))))

  ;; vector-length
  (check-42
    '(module
       (define L.vector-length.6
         (lambda (tmp.29)
           (if (vector? tmp.29) (unsafe-vector-length tmp.29) (error 9))))
       (define L.vector-init-loop.4
         (lambda (len.3 i.5 vec.4)
           (if (eq? len.3 i.5)
             vec.4
             (begin
               (unsafe-vector-set! vec.4 i.5 0)
               (call L.vector-init-loop.4 len.3 (unsafe-fx+ i.5 1) vec.4)))))
       (define L.make-init-vector.1
         (lambda (tmp.1)
           (if (unsafe-fx>= tmp.1 0)
             (let ((tmp.2 (unsafe-make-vector tmp.1)))
               (call L.vector-init-loop.4 tmp.1 0 tmp.2))
             (error 12))))
       (define L.make-vector.5
         (lambda (tmp.28)
           (if (fixnum? tmp.28) (call L.make-init-vector.1 tmp.28) (error 8))))
       (call L.vector-length.6 (call L.make-vector.5 42))))

  ;; vector-set!
  ;; vector-ref
  (check-42
    '(module
       (define L.unsafe-vector-ref.3
         (lambda (tmp.13 tmp.14)
           (if (unsafe-fx< tmp.14 (unsafe-vector-length tmp.13))
             (if (unsafe-fx>= tmp.14 0)
               (unsafe-vector-ref tmp.13 tmp.14)
               (error 11))
             (error 11))))
       (define L.vector-ref.7
         (lambda (tmp.35 tmp.36)
           (if (fixnum? tmp.36)
             (if (vector? tmp.35)
               (call L.unsafe-vector-ref.3 tmp.35 tmp.36)
               (error 11))
             (error 11))))
       (define L.unsafe-vector-set!.2
         (lambda (tmp.8 tmp.9 tmp.10)
           (if (unsafe-fx< tmp.9 (unsafe-vector-length tmp.8))
             (if (unsafe-fx>= tmp.9 0)
               (begin (unsafe-vector-set! tmp.8 tmp.9 tmp.10) (void))
               (error 10))
             (error 10))))
       (define L.vector-set!.6
         (lambda (tmp.32 tmp.33 tmp.34)
           (if (fixnum? tmp.33)
             (if (vector? tmp.32)
               (call L.unsafe-vector-set!.2 tmp.32 tmp.33 tmp.34)
               (error 10))
             (error 10))))
       (define L.vector-init-loop.4
         (lambda (len.5 i.7 vec.6)
           (if (eq? len.5 i.7)
             vec.6
             (begin
               (unsafe-vector-set! vec.6 i.7 0)
               (call L.vector-init-loop.4 len.5 (unsafe-fx+ i.7 1) vec.6)))))
       (define L.make-init-vector.1
         (lambda (tmp.3)
           (if (unsafe-fx>= tmp.3 0)
             (let ((tmp.4 (unsafe-make-vector tmp.3)))
               (call L.vector-init-loop.4 tmp.3 0 tmp.4))
             (error 12))))
       (define L.make-vector.5
         (lambda (tmp.30)
           (if (fixnum? tmp.30) (call L.make-init-vector.1 tmp.30) (error 8))))
       (let ((v.1 (call L.make-vector.5 2)))
         (let ((_.2 (call L.vector-set!.6 v.1 0 42))) (call L.vector-ref.7 v.1 0)))))

  ;; cons
  ;; car
  (check-42
    '(module
       (define L.car.6
         (lambda (tmp.35) (if (pair? tmp.35) (unsafe-car tmp.35) (error 12))))
       (define L.cons.5 (lambda (tmp.46 tmp.47) (cons tmp.46 tmp.47)))
       (call L.car.6 (call L.cons.5 42 0))))

  ;; cdr
  (check-42
    '(module
       (define L.cdr.6
         (lambda (tmp.36) (if (pair? tmp.36) (unsafe-cdr tmp.36) (error 13))))
       (define L.cons.5 (lambda (tmp.46 tmp.47) (cons tmp.46 tmp.47)))
       (call L.cdr.6 (call L.cons.5 0 42))))

  ;; Other M8 tests
  (check-42
    '(module
       (let ([v.1 (unsafe-make-vector 10)])
         (begin
           (begin
             (unsafe-vector-set! v.1 0 30)
             (unsafe-vector-set! v.1 1 2))
           (let ([a.1 (unsafe-fx+ (unsafe-vector-ref v.1 0) (unsafe-vector-ref v.1 1))])
             (unsafe-fx+ (unsafe-vector-length v.1) a.1))))))

  ;; TODO
  ;; is this even a valid program?
  (check-42
    '(module
       (if (unsafe-fx+ 10 2)
         42
         0)))
  ;; reference compiler doesn't like this one
  #;
  (check-42
    '(module
       (let ([v.1 (unsafe-make-vector 1)])
         (if (void? (unsafe-vector-set! v.1 0 42))
           42
           0))))

  ;; check vector length when length is an aloc
  (check-42
    '(module
       (let ([x.1 42])
         (let ([v.1 (unsafe-make-vector x.1)])
           (unsafe-vector-length v.1)))))

  ;; M9 tests
  (check-42
    '(module
       (define L.foo.1 (lambda () (void)))
       (unsafe-procedure-arity (make-procedure L.foo.1 42 0))))
  #;
  (check-eval-true
    '(module
       (define L.foo.1 (lambda () (void)))
       (eq? L.foo.1 (unsafe-procedure-label (make-procedure L.foo.1 0 0)))))
  (check-42
    '(module
       (define L.foo.1 (lambda () (void)))
       (let
         ([p.1 (make-procedure L.foo.1 0 3)]
          [idx.1 1]
          [idx.2 2]
          [v.1 12])
         (begin
           (unsafe-procedure-set! p.1 0 20)
           (unsafe-procedure-set! p.1 1 10)
           (unsafe-procedure-set! p.1 idx.2 v.1)
           (unsafe-fx+ (unsafe-fx+ (unsafe-procedure-ref p.1 0) (unsafe-procedure-ref p.1 idx.1))
                       (unsafe-procedure-ref p.1 2))))))

  (check-eval-false (safe-primop-module procedure? (void)))
  (check-eval-true
    '(module
       (define L.foo.1 (lambda () (void)))
       (let
         ([p.1 (make-procedure L.foo.1 0 3)])
         (procedure? p.1))))
  (check-eval-true
    '(module
       (define L.foo.1 (lambda () (void)))
       (procedure? (make-procedure L.foo.1 0 3))))
  (check-eval-false '(module (procedure? (void))))

  (check-true
    (procedure?
      (interp-exprs-bits-lang-v8
        (specify-representation
          '(module
             (define L.foo.1 (lambda () (void)))
             (unsafe-procedure-label (make-procedure L.foo.1 0 0)))))))
  )

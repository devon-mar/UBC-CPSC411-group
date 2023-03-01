#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2
  cpsc411/langs/v3)

(provide select-instructions)

;; MIlestone 2 Exercise 4
;;
;; Compiles Imp-cmf-lang v3 to Asm-lang v2, selecting appropriate sequences of
;; abstract assembly instructions to implement the operations of the source
;; language.
(define/contract (select-instructions p)
  (-> imp-cmf-lang-v3? asm-lang-v2?)

  (define (triv? t)
    (-> any/c boolean?)
    (or (name? t) (int64? t)))

  ;; Selects an appropriate asm-lang-v2 instruction
  ;; to implement the imp-cmf-lang-v3 instruction in e.
  ;;
  ;; imp-cmf-lang-v3 effect -> asm-lang-v2 effect
  (define (select-instructions-effect e)
    (match e
      ;; Modified template: Split set! case into two
      ;; cases based on the type of value.

      ;; optimization: We don't need to introduce a temp here
      ;; since asm-lang-v2 has (set! aloc_1 (binop aloc_1 triv)).
      ;; Therefore, if aloc and triv1 is the same, we don't have
      ;; to do anything.
      [`(set! ,aloc1 (,_ ,aloc2 ,_))
        ;; This is to remove unused variable warning.
        ;; Also makes it a bit more obvious what we're doing here.
        #:when (equal? aloc1 aloc2)
        e]
      ;; modified template - added a new case
      ;; We don't need to introduce a tmp if op1 is an aloc.
      ;; We can just add another set!
      [`(set! ,aloc (,binop ,op1 ,op2))
        `(begin
          (set! ,aloc ,op1)
          (set! ,aloc (,binop ,aloc ,op2)))]
      [`(set! ,aloc ,value)
        (select-instructions-value
          value
          (lambda (triv) `(set! ,aloc ,triv)))]
      ;; removed extra e since we assume input is valid
      [`(begin ,es ...)
        `(begin
           ,@(map select-instructions-effect es))]))

  ;; Selects an appropriate asm-lang-v2 instruction
  ;; to implement the imp-cmf-lang-v3 instruction in t.
  ;;
  ;; imp-cmf-lang-v3 tail -> asm-lang-v2 tail
  (define (select-instructions-tail t)
    (match t
      [`(begin ,es ... ,tail)
        `(begin
          ,@(map select-instructions-effect es)
          ,(select-instructions-tail tail))]
      [v
        (select-instructions-value
          v
          (lambda (triv) `(halt ,triv)))]))

  ;; If v is a binop, introduces a begin with the result of
  ;; (f triv) as the last statement, triv being some
  ;; temporary aloc which holds the result of the binop.
  ;; Otherwise returns (f v).
  ;;
  ;; v: imp-cmf-lang-v3 value
  ;; f: (triv? -> asm-lang-v2 tail or effect)
  (define (select-instructions-value v f)
    (match v
      [triv
       #:when (triv? triv)
       (f v)]
      ;; modified template to add new case
      [`(,binop ,t1 ,t2)
        (define tmp (fresh "tmp"))
        `(begin
           (set! ,tmp ,t1)
           (set! ,tmp (,binop ,tmp ,t2))
           ,(f tmp))]))

  (define (select-instructions-p p)
    (match p
      [`(module ,tail)
        `(module () ,(select-instructions-tail tail))]))

  (select-instructions-p p))

(module+ test
  (require rackunit)

  ;; Interp the output of select-instructions on i
  ;; and check that 42 is returned.
  (define (check-42 i)
    (-> asm-lang-v2? integer?)
    (check-equal?
      (interp-asm-lang-v2 (select-instructions i))
      42))


  ; simple
  (check-42 '(module 42))

  ; basic binop
  (check-42 '(module (+ 40 2)))

  ; basic set and return
  (check-42 '(module (begin (set! x.1 42) x.1)))

  ; add from two alocs and return result
  (check-42
   '(module
      (begin
        (set! x.1 40)
        (set! x.2 2)
        (+ x.1 x.2))))

  ; nested begins
  (check-42
    '(module
       (begin
         (begin
           (set! x.1 40)
           (set! x.2 2)
           (begin
             (set! x.1 (+ x.1 x.2))))
         x.1)))

  ;; nothing to change here
  (check-equal?
    (select-instructions
      '(module
         (begin
           (set! x.2 21)
           (set! x.2 (* x.2 2))
           x.2)))
    '(module
       ()
       (begin
         (set! x.2 21)
         (set! x.2 (* x.2 2))
         (halt x.2))))
  ;; same as above but just run it through check-42
  (check-42
    '(module
       (begin
         (set! x.2 21)
         (set! x.2 (* x.2 2))
         x.2)))

  (check-42
    '(module
       (begin
         (set! x.2 21)
         (set! z.3 (* x.2 2))
         z.3)))
  )

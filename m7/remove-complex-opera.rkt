#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  "../utils/gen-utils.rkt")

(provide remove-complex-opera*)

;; Milestone 7 Exercise 6
;; Milestone 8 Exercise 5
;;
;; Performs the monadic form transformation,
;; unnesting all non-trivial operators and operands to binops,
;; and calls making data flow explicit and simple to implement imperatively
(define/contract (remove-complex-opera* p)
  (-> exprs-bits-lang-v8? values-bits-lang-v8?)

  ;; exprs-bits-lang-v8-p -> values-bits-lang-v8-p
  (define (remove-complex-opera-p p)
    (match p
      [`(module (define ,ls (lambda (,as ...) ,vs)) ... ,value)
       `(module
          ,@(map remove-complex-opera-proc ls as vs)
          ,(remove-complex-opera-value value))]))

  ;; label (List-of aloc) exprs-bits-lang-v8-value -> values-bits-lang-v8-proc
  ;; values-bits-lang-v8-proc ::= (define label (lambda (aloc ...) tail))
  (define (remove-complex-opera-proc label alocs value)
    `(define ,label (lambda ,alocs ,(remove-complex-opera-value value))))

  ;; exprs-bits-lang-v8-pred -> values-bits-lang-v8-pred
  (define (remove-complex-opera-pred p)
    (match p
      [`(true) p]
      [`(false) p]
      [`(not ,pi)
       `(not ,(remove-complex-opera-pred pi))]
      [`(let ([,as ,vs] ...) ,pt)
       (define new-vs (map remove-complex-opera-value vs))
       `(let
         ,(map list as new-vs)
         ,(remove-complex-opera-pred pt))]
      [`(if ,pred ,p1 ,p2)
       `(if
         ,(remove-complex-opera-pred pred)
         ,(remove-complex-opera-pred p1)
         ,(remove-complex-opera-pred p2))]
      [`(begin ,effects ... ,pred)
       `(begin
         ,@(map remove-complex-opera-effect effects)
         ,(remove-complex-opera-pred pred))]
      [`(,relop ,v1 ,v2)
       ;; needs (relop opand opand)
       (use-let-values
         (list opand? opand?)
         (map remove-complex-opera-value (list v1 v2))
         (lambda (opand1 opand2)
           `(,relop ,opand1 ,opand2)))]))

  ;; exprs-bits-lang-v8-value -> values-bits-lang-v8-value
  (define (remove-complex-opera-value v)
    (match v
      [`(mref ,v1 ,v2)
       ;; needs (mref aloc opand)
       (use-let-values
         (list aloc? opand?)
         (map remove-complex-opera-value (list v1 v2))
         (lambda (aloc opand)
           `(mref ,aloc ,opand)))]
      [`(alloc ,value)
       ;; needs (alloc opand)
       (use-let-values
         (list opand?)
         (list (remove-complex-opera-value value))
         (lambda (opand)
           `(alloc ,opand)))]
      [`(call ,vc ,vs ...)
       ;; needs (call triv opand ...)
       (use-let-values
         (list* triv? (make-list (length vs) opand?))
         (map remove-complex-opera-value (list* vc vs))
         (lambda vals
           (define triv (first vals))
           (define opands (rest vals))
           `(call ,triv ,@opands)))]
      [`(let ([,as ,vs] ...) ,vt)
       (define new-vs (map remove-complex-opera-value vs))
       `(let
         ,(map list as new-vs)
         ,(remove-complex-opera-value vt))]
      [`(if ,pred ,v1 ,v2)
       `(if
         ,(remove-complex-opera-pred pred)
         ,(remove-complex-opera-value v1)
         ,(remove-complex-opera-value v2))]
      [`(begin ,effects ... ,value)
       `(begin
         ,@(map remove-complex-opera-effect effects)
         ,(remove-complex-opera-value value))]
      [`(,binop ,v1 ,v2)
       ;; needs (binop opand opand)
       (use-let-values
         (list opand? opand?)
         (map remove-complex-opera-value (list v1 v2))
         (lambda (opand1 opand2)
           `(,binop ,opand1 ,opand2)))]
      [triv triv]))

  ;; exprs-bits-lang-v8-effect -> values-bits-lang-v8-effect
  (define (remove-complex-opera-effect e)
    (match e
      [`(mset! ,v1 ,v2 ,v3)
       ;; needs (mset! aloc opand value)
       (define new-vs (map remove-complex-opera-value (list v1 v2 v3)))
       (use-let-values
         (list aloc? opand? (lambda (_) #t))
         new-vs
         (lambda (aloc opand value)
           `(mset! ,aloc ,opand ,value)))]
      [`(begin ,effects ... ,effect-t)
       `(begin
          ,@(map remove-complex-opera-effect effects)
          ,(remove-complex-opera-effect effect-t))]))

  ;; (List-of (exprs-bits-lang-v8-value -> boolean))
  ;; (List-of exprs-bits-lang-v8-value)
  ;; (exprs-bits-lang-v8-value ... -> values-bits-lang-v8-value|effect)
  ;; -> values-bits-lang-v8-value|effect
  ;; Use lets to bind unsupported values to alocs
  ;; by taking in a list of checks that checks whether a value is supported in Values Bits Lang,
  ;; a list of supported/unsupported values that are inside of the outer structure
  ;; and a function that will create the outer structure from supported values.
  (define (use-let-values check-list vs fn)
    (define-values (args aloc-vs)
      (for/fold ([args '()]
                 [aloc-vs '()])
                ([v vs]
                 [check? check-list])
        (if (check? v)
            (values (append-e args v)
                    aloc-vs)
            (let ([new-aloc (fresh)])
              (values (append-e args new-aloc)
                      (append-e aloc-vs (list new-aloc v)))))))
    (if (empty? aloc-vs)
        (apply fn args)
        `(let ,aloc-vs ,(apply fn args))))

  ;; any -> boolean
  ;; Returns true iff o is a opand in Values Bits Lang
  (define (opand? o)
    (or (aloc? o) (int64? o)))

  ;; any -> boolean
  ;; Returns true if t is a triv in Values Bits Lang
  (define (triv? t)
    (or (label? t) (opand? t)))

  (remove-complex-opera-p p))

(module+ test
  (require rackunit)

  ;; Check that the compiled program interprets to 42
  (define-check (check-42 p)
    (check-equal?
      (interp-values-bits-lang-v8 (remove-complex-opera* p))
      42))

  ;; Check that compiled program is the same as the original program
  ;; and interprets to 42
  (define-check (check-no-change-42 p)
    (define compiled (remove-complex-opera* p))
    (check-equal? compiled p)
    (check-equal?
      (interp-values-bits-lang-v8 compiled)
      42))

  ;; base case
  (check-42 '(module 42))
  (check-42 '(module (+ 37 5)))
  ;; no let for label in call
  (check-no-change-42
    '(module
      (define L.test.1 (lambda (a.1) (+ a.1 a.1)))
      (call L.test.1 21)))

  ;; label as value
  (check-42
    '(module
      (define L.test.1 (lambda (a.1 b.1 c.1) (+ (call a.1) (call b.1))))
      (define L.test.2 (lambda () 22))
      (define L.test.3 (lambda () (* (call L.test.4) -10)))
      (define L.test.4 (lambda () -2))
      (call L.test.1 L.test.2 L.test.3 L.test.4)))

  ;; complex binop
  (check-42
    '(module
      (define L.test.1 (lambda (a.1) (* (+ 8 (call L.test.2 a.1)) 2)))
      (define L.test.2 (lambda (b.2) (+ b.2 b.2)))
      (+ (let ([x.1 5] [y.1 2])
           (* (if (true) x.1 y.1) y.1))
         (call L.test.1 4))))

  ;; complex relop
  (check-42
    '(module
      (define L.test.1
        (lambda (a.1 b.2)
          (if (if (= (+ 18 (- 4 2)) 20)
                  (let ([y.2 9]) (> (let ([x.1 10]) (+ a.1 x.1)) (* b.2 10)))
                  (not (false)))
              -20
              19)))
      (if (not (>= (if (not (> 4 5)) 18 22)
                   (call L.test.1 4 (let ([x.1 2] [y.1 -3]) (- x.1 y.1)))))
        42
        -1)))

  ;; complex call
  (check-42
    '(module
      (define L.test.1 (lambda (a.1 b.1 c.1 d.1) (+ (* (+ a.1 (* 2 c.1)) b.1) d.1)))
      (define L.test.2 (lambda (a.1 b.1) (call L.test.3 (- a.1 b.1))))
      (define L.test.3 (lambda (a.1) a.1))
      (call
        (let ([x.1 L.test.1]) x.1)
        (+ 3 2)
        (call L.test.2 9 4)
        -1
        (if (not (true))
            -2
            (* (call L.test.2 (+ 22 44) 63)
               (call (call L.test.3 L.test.2) 5 -4))))))

  ;; value in let handled
  (check-42
    '(module
      (define L.test.1 (lambda (a.1) (- a.1 1)))
      (* (let ([x.1 (+ (- 40 15) (- 2 (call L.test.1 (let ([x.2 (+ 10 (+ 1 1))]) x.2))))]
               [y.2 (if (let ([x.1 (call L.test.1 (* 2 5))]) (= x.1 9))
                        (call L.test.1 (- 8 2))
                        (call L.test.1 200))])
           (+ x.1 y.2))
         2)))

  ;; begin in pred
  (check-42
    '(module
      (let ([x.1 (alloc 8)]
            [y.1 (alloc 16)])
        (if (begin
              (mset! x.1 0 (* 7 6))
              (begin
                (mset! y.1 (+ 6 2) (* 6 6))
                (true)))
            (mref x.1 (- 36 (mref y.1 8)))
            -1))))

  ;; begin in value/effect
  (check-42
    '(module
      (let ([x.1 (alloc 16)])
        (begin
          (begin (mset! x.1 (let () 8) 42))
          (mref x.1 (+ (* 2 2) 4))))))

  ;; alloc, mref, mset
  (check-42
    '(module
      (let ([x.1 (alloc (* 8 2))]
            [y.1 (alloc (if (false) 8 32))])
        (begin
          (mset!
            (let () x.1)
            (begin
              (mset! (if (true) y.1 -1) (- 2 2) (let ([x.1 8]) x.1))
              (mref (if (not (not (false))) -1 y.1) (let ([y.1 0]) y.1)))
            (* 2 21))
          (mref (let () x.1) (* 2 4))))))

  ;; valid alloc, mref, mset - no change
  (check-no-change-42
    '(module
       (let ([x.1 (alloc 16)])
        (begin
          (mset! x.1 8 42)
          (mref x.1 8)))))
  )

#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

(provide check-values-lang)

;; Milestone 4 Exercise 22
;;
;; Takes an arbitrary value and either returns it, if it is a valid Values-lang
;; v4 program, or raises an error with a descriptive error message.
(define/contract (check-values-lang p)
  (-> any/c values-lang-v4?)

  (define/contract (check-values-lang-p env p)
    (-> generic-set? any/c values-lang-v4?)
    (match p
      [`(module ,tail)
        (check-values-lang-tail env tail)
        p]
      [_ (error "Program must begin with a module")]))

  ;; Throws an error if names contains duplicate names.
  (define/contract (check-duplicate-bindings names)
    (-> (listof name?) void?)
    (if (and (check-duplicates names) #t)
      (error "Duplicate parallel bindings found")
      (void)))


  ;; Check the xs and vs from a let.
  ;; If valid, return void or raise an error.
  (define/contract (check-let env xs vs)
    (-> generic-set? any/c any/c void?)
    (unless (andmap name? xs)
      (error "invalid xs in let"))
    (for ([v vs])
      (check-values-lang-value env v))
    (check-duplicate-bindings xs))

  (define/contract (relop? r)
    (-> any/c boolean?)
    (and (member r '(< <= = >= > !=))
         #t))

  (define/contract (binop? b)
    (-> any/c boolean?)
    (and
      (member b '(* +))
      #t))

  (define/contract (check-values-lang-pred env p)
    (-> generic-set? any/c void?)
    (match p
      [`(true)
        (void)]
      [`(false)
        (void)]
      [`(not ,pred)
        (check-values-lang-pred env pred)]
      [`(let ([,xs ,vs] ...) ,pred)
        (check-let env xs vs)
        (check-values-lang-pred (set-union env xs) pred)]
      [`(if ,p1 ,p2 ,p3)
        (check-values-lang-pred env p1)
        (check-values-lang-pred env p2)
        (check-values-lang-pred env p3)]
      [`(,relop ,t1 ,t2)
        (unless (relop? relop)
          (error "invalid relop " relop))
        (check-values-lang-triv env t1)
        (check-values-lang-triv env t2)]
      [_ (error "invalid pred " p)]))

  (define/contract (check-values-lang-tail env t)
    (-> generic-set? any/c void?)
    (match t
      [`(let ([,xs ,vs] ...) ,tail)
        (check-let env xs vs)
        (check-values-lang-tail (set-union env xs) tail)]
      [`(if ,pred ,t1 ,t2)
        (check-values-lang-pred env pred)
        (check-values-lang-tail env t1)
        (check-values-lang-tail env t2)]
      [_ (check-values-lang-value env t)]))

  (define/contract (check-values-lang-value env v)
    (-> generic-set? any/c void?)
    (match v
      [`(let ([,xs ,vs] ...) ,value)
        (check-let env xs vs)
        (check-values-lang-value (set-union env xs) value)]
      [`(if ,pred ,v1 ,v2)
        (check-values-lang-pred env pred)
        (check-values-lang-value env v1)
        (check-values-lang-value env v2)]
      [`(,binop ,t1 ,t2)
        (unless (binop? binop)
        (error "invalid binop " binop))
        (check-values-lang-tail env t1)
        (check-values-lang-tail env t2)]
      [_ (check-values-lang-triv env v)]))

  (define/contract (check-values-lang-triv env t)
    (-> generic-set? any/c void?)
    (match t
      [(? int64?) (void)]
      [(? name?)
       (unless (set-member? env t)
         (error "undeclared name: " t))]
      [_ (error "invalid triv " t)]))

  (check-values-lang-p '() p))

(module+ test
  (require rackunit)

  (define (check-valid p)
    (check-equal? (check-values-lang p) p)
    (check-true (values-lang-v4? p)))

  (define (check-invalid p)
    (check-exn exn:fail? (lambda () (check-values-lang p) p)))

  ;; From M2
  ;; minimal valid programs
  (check-valid
    '(module 42))
  (check-valid
    '(module (+ 40 2)))
  (check-valid
    '(module (let ([x 42]) x)))

  ; minimal invalid programs
  (check-invalid
    '(module "42"))
  (check-invalid
    '(notmodule 42))

  ;; x not defined
  (check-invalid
    '(module x))
  (check-invalid
    '(module (let ([x "not a number"]) x)))
  (check-invalid
    '(module (/ 40 2)))

  ;; invalid name
  (check-invalid
    '(module (let (["x" 42]) x)))

  ;; duplicate parallel bindings
  (check-invalid
    '(module (let ([x 42] [x 40]) x)))
  (check-invalid
    '(module (let ([x 42] [y 41] [x 40]) x)))
  (check-invalid
    '(module
       (let
         ([x 42]
          [y (let ([z 42] [z 42]) y)])
         x)))

  ;; some more complicated tests
  (check-valid
    '(module
       (let ([x 42]
             [y (let ([x 20]) x)])
       x)))
  (check-valid
    '(module
       (let ([y 42])
         (let ([x y])
           x))))
  (check-invalid
    '(module
       (let ([y (let ([x 42]) x)])
         x)))
  (check-invalid
    '(module
       (let ([x 42]
             [y x])
         y)))


  ;; M3 tests
  (check-valid
    '(module
       (if (if (not (false)) (true) (= 1 2))
         42
         42)))

  ;; invalid relop
  (check-invalid
    '(module
       (if (== 1 2)
         0
         42)))

  ;; invalid pred
  (check-invalid
    '(module
       (if (t)
         0
         42)))

  ;; duplicate binding in pred let
  (check-invalid
    '(module
       (if (let ([x 10]
                 [x 2])
             (= x 2))
         0
         42)))

  ;; undefined var in pred let
  (check-invalid
    '(module
       (if (let ([x 10]
                 [z 2])
             (= y 2))
         0
         42)))

  ;; value if
  (check-valid
    '(module
       (let ([x (if (true) 42 0)])
         x)))
  )

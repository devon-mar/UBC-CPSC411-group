#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2)

(provide uncover-locals)

;; Milestone 2 Exercise 6
;;
;; Compiles Asm-lang v2 to Asm-lang v2/locals, analysing which abstract
;; locations are used in the program and decorating the program with the
;; set of variables in an info field.
(define/contract (uncover-locals p)
  (-> asm-lang-v2? asm-lang-v2/locals?)

  ;; Returns the set of trivs used in t.
  ;;
  ;; t: asm-lang-v2-tail
  ;; -> set?
  (define/contract (uncover-locals-triv t)
    (-> any/c set?)
    (match t
      [int64 #:when (int64? int64) (set)]
      [aloc (set aloc)]))

  ;; Returns the set of trivs used in e.
  ;;
  ;; e: asm-lang-v2-effect
  ;; -> set?
  (define/contract (uncover-locals-effect e)
    (-> any/c set?)
    (match e
      [`(set! ,aloc (,_ ,aloc ,triv)) (set-add (uncover-locals-triv triv) aloc)]
      [`(set! ,aloc ,triv) (set-add (uncover-locals-triv triv) aloc)]
      [`(begin ,es ..., e)
        (foldr set-union (uncover-locals-effect e) (map uncover-locals-effect es))]))

  ;; Returns the set of trivs used in t.
  ;;
  ;; t: asm-lang-v2-tail
  ;; -> set?
  (define/contract (uncover-locals-tail t)
    (-> any/c set?)
    (match t
      [`(halt ,triv) (uncover-locals-triv triv)]
      [`(begin ,es ... ,tail)
        (foldl set-union (uncover-locals-tail tail) (map uncover-locals-effect es))]))

  ;; asm-lang-v2-p -> asm-lang-v2/locals-p
  (define (uncover-locals-p p)
    (match p
      [`(module ,info ,tail)
        `(module 
           ,(info-set info 'locals (set->list (uncover-locals-tail tail)))
           ,tail)]))

  (uncover-locals-p p))


(module+ test
  (require rackunit)

  ; no locals
  (check-equal?
    (uncover-locals '(module () (begin (halt 0))))
    '(module ((locals ())) (begin (halt 0))))

  ; 1 local
  (check-equal?
    (uncover-locals
     '(module ()
       (begin
         (set! x.1 0)
         (halt x.1))))
   '(module ((locals (x.1)))
     (begin
       (set! x.1 0)
       (halt x.1))))
  ; 2 locals
  (check-match
    (uncover-locals
     '(module ()
       (begin
         (set! x.1 0)
         (set! y.1 x.1)
         (set! y.1 (+ y.1 x.1))
         (halt y.1))))
   `(module ((locals (,locals ...)))
     (begin
       (set! x.1 0)
       (set! y.1 x.1)
       (set! y.1 (+ y.1 x.1))
       (halt y.1)))
   (and
     (member 'x.1 locals)
     (member 'y.1 locals)
     (= (length locals) 2)))

  ; nested begin
  (check-match
    (uncover-locals
     '(module ()
       (begin
         (begin
           (set! x.1 0)
           (set! y.1 x.1))
         (set! y.1 (+ y.1 x.1))
         (halt y.1))))
   `(module ((locals (,locals ...)))
     (begin
       (begin
         (set! x.1 0)
         (set! y.1 x.1))
       (set! y.1 (+ y.1 x.1))
       (halt y.1)))
   (and
     (member 'x.1 locals)
     (member 'y.1 locals)
     (= (length locals) 2)))

  ;; Info in input should be preserved.
  (check-equal?
    (uncover-locals '(module ([test 123]) (begin (halt 0))))
    `(module ,(info-set '([test 123]) 'locals '()) (begin (halt 0))))
  )

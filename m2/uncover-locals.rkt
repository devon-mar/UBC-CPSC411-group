#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4
  "../utils/compiler-utils.rkt")

(provide uncover-locals)

;; Milestone 2 Exercise 6
;; Milestone 4 Exercise 16
;;
;; Compiles Asm-pred-lang v4 to Asm-pred-lang v4/locals, analysing which abstract
;; locations are used in the program and decorating the program with the
;; set of variables in an info field.
(define/contract (uncover-locals p)
  (-> asm-pred-lang-v4? asm-pred-lang-v4/locals?)

  ;; Returns the set of alocs used in t.
  ;;
  ;; t: asm-pred-lang-v4-tail
  ;; -> set?
  (define/contract (uncover-locals-triv t)
    (-> any/c set?)
    (match t
      [int64 #:when (int64? int64) (set)]
      [aloc (set aloc)]))

  ;; Returns the set of alocs used in p.
  ;;
  ;; p: asm-pred-lang-v4-pred
  ;; -> set?
  (define/contract (uncover-locals-pred p)
    (-> any/c set?)
    (match p
 	 	  [`(true) (set)]
 	 	  [`(false) (set)]
 	 	  [`(not ,pred) (uncover-locals-pred pred)]
 	 	  [`(begin ,effect ... ,pred)
       (foldl set-union (uncover-locals-pred pred) (map uncover-locals-effect effect))]
      [`(,relop ,aloc ,triv)
       #:when(relop? relop)
       (set-add (uncover-locals-triv triv) aloc)]
 	 	  [`(if ,ppred ,pred1 ,pred2)
       (set-union
         (uncover-locals-pred ppred)
         (uncover-locals-pred pred1)
         (uncover-locals-pred pred2))]))

  ;; Returns the set of alocs used in e.
  ;;
  ;; e: asm-pred-lang-v4-effect
  ;; -> set?
  (define/contract (uncover-locals-effect e)
    (-> any/c set?)
    (match e
      [`(set! ,aloc (,_ ,aloc ,triv)) (set-add (uncover-locals-triv triv) aloc)]
      [`(set! ,aloc ,triv) (set-add (uncover-locals-triv triv) aloc)]
      [`(begin ,es ..., e)
       (foldr set-union (uncover-locals-effect e) (map uncover-locals-effect es))]
      [`(if ,pred ,effect1 ,effect2)
       (set-union
         (uncover-locals-pred pred)
         (uncover-locals-effect effect1)
         (uncover-locals-effect effect2))]))

  ;; Returns the set of alocs used in t.
  ;;
  ;; t: asm-pred-lang-v4-tail
  ;; -> set?
  (define/contract (uncover-locals-tail t)
    (-> any/c set?)
    (match t
      [`(halt ,triv) (uncover-locals-triv triv)]
      [`(begin ,es ... ,tail)
       (foldl set-union (uncover-locals-tail tail) (map uncover-locals-effect es))]
      [`(if ,pred ,tail1 ,tail2)
       (set-union
         (uncover-locals-pred pred)
         (uncover-locals-tail tail1)
         (uncover-locals-tail tail2))]))

  ;; asm-pred-lang-v4-p -> asm-pred-lang-v4/locals-p
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

  ;; aloc in ifs
  (check-match
    (uncover-locals
      `(module ()
        (begin
          (begin
            (set! x.1 0)
            (if (true) (set! a.1 x.1) (set! b.1 x.1)))
          (begin
            (if (false)
                (set! c.1 y.1)
                (set! d.1 y.1))
            (if (not (begin (if (false) (set! e.1 z.1) (set! f.1 z.1)) (< v.1 2)))
                (begin
                  (if (not (false))
                      (set! g.1 x.1)
                      (set! h.1 x.1))
                  (halt x.1))
                (begin
                  (if (= x.1 x.1)
                      (set! i.1 x.1)
                      (set! j.1 x.1))
                  (if (true) (halt k.1) (halt l.1))))))))
    `(module ((locals (,locals ...)))
      (begin
        (begin
          (set! x.1 0)
          (if (true) (set! a.1 x.1) (set! b.1 x.1)))
        (begin
          (if (false)
              (set! c.1 y.1)
              (set! d.1 y.1))
          (if (not (begin (if (false) (set! e.1 z.1) (set! f.1 z.1)) (< v.1 2)))
              (begin
                (if (not (false))
                    (set! g.1 x.1)
                    (set! h.1 x.1))
                (halt x.1))
              (begin
                (if (= x.1 x.1)
                    (set! i.1 x.1)
                    (set! j.1 x.1))
                (if (true) (halt k.1) (halt l.1)))))))
    (equal? (list->set locals)
            (list->set '(a.1 b.1 c.1 d.1 e.1 f.1 g.1 h.1 i.1 j.1 k.1 l.1 v.1 x.1 y.1 z.1))))

  ;; aloc in pred
  (check-match
    (uncover-locals
      `(module ()
        (begin
          (set! x.1 0)
          (if (not (not (begin (if (= a.1 b.1) (set! c.1 d.1) (set! e.1 f.1)) (> g.1 h.1))))
              (begin
                (if (not (<= i.1 j.1))
                    (set! x.1 2)
                    (set! x.1 3))
                (halt x.1))
              (begin
                (if (= k.1 l.1)
                    (set! x.1 4)
                    (set! x.1 5))
                (if (begin (!= m.1 n.1)) (halt 6) (halt 7)))))))
    `(module ((locals (,locals ...)))
      (begin
        (set! x.1 0)
        (if (not (not (begin (if (= a.1 b.1) (set! c.1 d.1) (set! e.1 f.1)) (> g.1 h.1))))
            (begin
              (if (not (<= i.1 j.1))
                  (set! x.1 2)
                  (set! x.1 3))
              (halt x.1))
            (begin
              (if (= k.1 l.1)
                  (set! x.1 4)
                  (set! x.1 5))
              (if (begin (!= m.1 n.1)) (halt 6) (halt 7))))))
    (equal? (list->set locals)
            (list->set '(a.1 b.1 c.1 d.1 e.1 f.1 g.1 h.1 i.1 j.1 k.1 l.1 m.1 n.1 x.1))))
  )

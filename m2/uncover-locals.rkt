#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5)

(provide uncover-locals)

;; Milestone 2 Exercise 6
;; Milestone 4 Exercise 16
;; Milestone 5 Exercise 7
;;
;; Compiles Asm-pred-lang v5 to Asm-pred-lang v5/locals, analysing which abstract
;; locations are used in the program and decorating the program with the
;; set of variables in an info field.
(define/contract (uncover-locals p)
  (-> asm-pred-lang-v5? asm-pred-lang-v5/locals?)

  ;; Returns a procedure with the 'locals info set.
  ;; 
  ;; label: procedure label
  ;; tail: procedure tail
  ;; -> asm-pred-lang-v5/locals-proc
  (define (uncover-locals-proc label info tail)
    (-> label? info? any/c any/c)
    `(define
       ,label
       ,(info-set info 'locals (set->list (uncover-locals-tail tail)))
       ,tail))

  ;; asm-pred-lang-v5-p -> asm-pred-lang-v5/locals-p
  (define (uncover-locals-p p)
    (match p
      [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
        `(module
           ,(info-set info 'locals (set->list (uncover-locals-tail tail)))
           ,@(map uncover-locals-proc labels infos tails)
           ,tail)]))

  ;; asm-pred-lang-v5-pred -> set
  (define/contract (uncover-locals-pred p)
    (-> any/c set?)
    (match p
      [`(true) (set)]
      [`(false) (set)]
      [`(not ,p) (uncover-locals-pred p)]
      [`(begin ,es ... ,p)
        (foldl set-union (uncover-locals-pred p) (map uncover-locals-effect es))]
      [`(if ,p1 ,p2 ,p3)
        (set-union
          (uncover-locals-pred p1)
          (uncover-locals-pred p2)
          (uncover-locals-pred p3))]
      [`(,_ ,l ,o)
        (set-union
          (uncover-locals-loc l)
          (uncover-locals-opand o))]))

  ;; asm-pred-lang-v5-tail -> set
  (define/contract (uncover-locals-tail t)
    (-> any/c set?)
    (match t
      [`(halt ,o)
        (uncover-locals-opand o)]
      [`(jump ,trg ,_ ...)
        (uncover-locals-trg trg)]
      [`(begin ,es ... ,t)
        (foldl
          set-union
          (uncover-locals-tail t)
          (map uncover-locals-effect es))]
      [`(if ,p ,t1 ,t2)
        (set-union
          (uncover-locals-pred p)
          (uncover-locals-tail t1)
          (uncover-locals-tail t2))]))

  ;; asm-pred-lang-v5-effect -> set
  (define (uncover-locals-effect e)
    (match e
      [`(set! ,loc (,_ ,loc ,opand))
        (set-union
          (uncover-locals-loc loc)
          (uncover-locals-opand opand))]
      [`(set! ,loc ,triv)
        (set-union
          (uncover-locals-loc loc)
          (uncover-locals-triv triv))]
      ;; modified template - removed tail effect
      [`(begin ,es ...) 
        (foldl
          set-union
          (set)
          (map uncover-locals-effect es))]
      [`(if ,p ,e1 ,e2)
        (set-union
          (uncover-locals-pred p)
          (uncover-locals-effect e1)
          (uncover-locals-effect e2))]))

  ;; asm-pred-lang-v5-opand -> set
  (define/contract (uncover-locals-opand o)
    (-> any/c set?)
    (match o
      [(? int64?) (set)]
      ;; loc
      [_ (uncover-locals-loc o)]))

  ;; asm-pred-lang-v5-triv -> set
  (define/contract (uncover-locals-triv t)
    (-> any/c set?)
    (match t
      [(? label?) (set)]
      [_ (uncover-locals-opand t)]))

  ;; asm-pred-lang-v5-loc -> set
  (define/contract (uncover-locals-loc l)
    (-> any/c set?)
    (match l
      [(? aloc?) (set l)]
      ;; rloc
      [_ (set)]))

  ;; asm-pred-lang-v5-trg -> set
  (define/contract (uncover-locals-trg t)
    (-> any/c set?)
    (match t
      [(? label?)
       (set)]
      ;; loc
      [_ (uncover-locals-loc t)]))

  #;
  (define (uncover-locals-binop b)
    (match b
      ['* (void)]
      ['+ (void)]))
  
  #;
  (define (uncover-locals-relop r)
    (match r
      ['< (void)]
      ['<= (void)]
      ['= (void)]
      ['>= (void)]
      ['> (void)]
      ['!= (void)]))

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

  ;; M5 test
  (define m5-locals-1 (list->set '(a.1 b.1 c.1 d.1 e.1 f.1 g.1 h.1 i.1 j.1 k.1 l.1 m.1 n.1 x.1)))
  (define m5-tail-1
    '(begin
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
  (define m5-locals-2 (list->set '(a.1 b.1 c.1 d.1 e.1 f.1 g.1 h.1 i.1 j.1 k.1 l.1 v.1 x.1 y.1 z.1)))
  (define m5-tail-2 
    '(begin
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
  (check-match
    (uncover-locals
      `(module ()
         (define L.foo.1 () ,m5-tail-1)
         (define L.bar.1 () ,m5-tail-2)
         ,m5-tail-1))
    `(module
       ,info
       (define L.foo.1 ,foo-info ,foo-tail)
       (define L.bar.1 ,bar-info ,bar-tail)
       ,tail)
    (and
      (equal? tail m5-tail-1)
      (equal? foo-tail m5-tail-1)
      (equal? bar-tail m5-tail-2)
      (equal? (list->set (info-ref foo-info 'locals)) m5-locals-1)
      (equal? (list->set (info-ref bar-info 'locals)) m5-locals-2)
      (equal? (list->set (info-ref info 'locals)) m5-locals-1)))

  (define fbp (current-frame-base-pointer-register))
  (check-match
    (uncover-locals
      `(module
         ()
         (define L.foo.1 () (halt 42))
         (define L.bar.1 () (halt 2))
         (begin
           (set! a.1 1)
           (set! b.1 2)
           (set! rax 10)
           (set! rax (* rax a.1))
           (set! a.1 (+ a.1 1))
           (set! a.1 (+ a.1 b.1))
           (set! x.1 L.bar.1)
           (if
             (if (true) (false) (false))
             (jump L.foo.1 ,fbp)
             (jump x.1 ,fbp)))))
    `(module
       ,info
       (define L.foo.1 ,foo-info (halt 42))
       (define L.bar.1 ,bar-info (halt 2))
       (begin
         (set! a.1 1)
         (set! b.1 2)
         (set! rax 10)
         (set! rax (* rax a.1))
         (set! a.1 (+ a.1 1))
         (set! a.1 (+ a.1 b.1))
         (set! x.1 L.bar.1)
         (if
           (if (true) (false) (false))
           (jump L.foo.1 ,fbp1)
           (jump x.1 ,fbp1))))
    (and (equal? fbp1 fbp)
         (set-empty? (info-ref foo-info 'locals))
         (set-empty? (info-ref bar-info 'locals))
         (set=? (info-ref info 'locals) '(a.1 b.1 x.1))))

  )

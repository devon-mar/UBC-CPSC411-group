#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide uncover-locals)

;; Milestone 2 Exercise 6
;; Milestone 4 Exercise 16
;; Milestone 5 Exercise 7
;; Milestone 6 Exercise 7
;; Milestone 7 Exercise 7
;; Milestone 8 Exercise 11
;;
;; Compiles Asm-pred-lang v8 to Asm-pred-lang v8/locals, analysing which abstract
;; locations are used in the program and decorating the program with the
;; set of variables in an info field.
(define/contract (uncover-locals p)
  (-> asm-pred-lang-v8? asm-pred-lang-v8/locals?)

  ;; Returns a procedure with the 'locals info set.
  ;;
  ;; label: procedure label
  ;; tail: procedure tail
  ;; -> asm-pred-lang-v8/locals-proc
  (define (uncover-locals-proc label info tail)
    (-> label? info? any/c any/c)
    `(define
       ,label
       ,(info-set info 'locals (set->list (uncover-locals-tail tail)))
       ,tail))

  ;; asm-pred-lang-v8-p -> asm-pred-lang-v8/locals-p
  (define (uncover-locals-p p)
    (match p
      [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
        `(module
           ,(info-set info 'locals (set->list (uncover-locals-tail tail)))
           ,@(map uncover-locals-proc labels infos tails)
           ,tail)]))

  ;; asm-pred-lang-v8-pred -> set
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
      [`(,_relop ,l ,o)
        (set-union
          (uncover-locals-loc l)
          (uncover-locals-opand o))]))

  ;; asm-pred-lang-v8-tail -> set
  (define/contract (uncover-locals-tail t)
    (-> any/c set?)
    (match t
      [`(jump ,trg ,_loc ...)
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

  ;; asm-pred-lang-v8-effect -> set
  (define (uncover-locals-effect e)
    (match e
      [`(return-point ,_label ,tail)
        (uncover-locals-tail tail)]
      [`(set! ,loc1 (mref ,loc2 ,index))
        (set-union
          (uncover-locals-loc loc1)
          (uncover-locals-loc loc2)
          (uncover-locals-index index))]
      [`(mset! ,loc ,index ,triv)
        (set-union
          (uncover-locals-loc loc)
          (uncover-locals-index index)
          (uncover-locals-triv triv))]
      [`(set! ,loc (,_binop ,loc ,opand))
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

  ;; asm-pred-lang-v8-opand -> set
  (define/contract (uncover-locals-opand o)
    (-> any/c set?)
    (match o
      [(? int64?) (set)]
      ;; loc
      [_ (uncover-locals-loc o)]))


  ;; asm-pred-lang-v8-index -> set
  (define/contract (uncover-locals-index i)
    (-> any/c set?)
    (match i
      [(? int64?) (set)]
      ;; loc
      [_ (uncover-locals-loc i)]))

  ;; asm-pred-lang-v8-triv -> set
  (define/contract (uncover-locals-triv t)
    (-> any/c set?)
    (match t
      [(? label?) (set)]
      [_ (uncover-locals-opand t)]))

  ;; asm-pred-lang-v8-loc -> set
  (define/contract (uncover-locals-loc l)
    (-> any/c set?)
    (match l
      [(? aloc?) (set l)]
      ;; rloc
      [_ (set)]))

  ;; asm-pred-lang-v8-trg -> set
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
      ['+ (void)]
      ['- (void)]
      ['bitwise-and (void)]
      ['bitwise-ior (void)]
      ['bitwise-xor (void)]
      ['arithmetic-shift-right (void)]))

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
    (uncover-locals '(module ((new-frames ())) (begin (jump r15))))
    '(module ((new-frames ()) (locals ())) (begin (jump r15))))

  ; 1 local
  (check-equal?
    (uncover-locals
     '(module ((new-frames ()))
       (begin
         (set! x.1 0)
         (jump x.1))))
   '(module ((new-frames ()) (locals (x.1)))
     (begin
       (set! x.1 0)
       (jump x.1))))
  ; 2 locals
  (check-match
    (uncover-locals
     '(module ((new-frames ()))
       (begin
         (set! x.1 0)
         (set! y.1 x.1)
         (set! y.1 (+ y.1 x.1))
         (jump y.1))))
   `(module ((new-frames ()) (locals (,locals ...)))
     (begin
       (set! x.1 0)
       (set! y.1 x.1)
       (set! y.1 (+ y.1 x.1))
       (jump y.1)))
   (and
     (member 'x.1 locals)
     (member 'y.1 locals)
     (= (length locals) 2)))

  ; nested begin
  (check-match
    (uncover-locals
     '(module ((new-frames ()))
       (begin
         (begin
           (set! x.1 0)
           (set! y.1 x.1))
         (set! y.1 (+ y.1 x.1))
         (jump y.1))))
   `(module ((new-frames ()) (locals (,locals ...)))
     (begin
       (begin
         (set! x.1 0)
         (set! y.1 x.1))
       (set! y.1 (+ y.1 x.1))
       (jump y.1)))
   (and
     (member 'x.1 locals)
     (member 'y.1 locals)
     (= (length locals) 2)))

  ;; aloc in ifs
  (check-match
    (uncover-locals
      `(module ((new-frames ()))
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
                  (jump x.1))
                (begin
                  (if (= x.1 x.1)
                      (set! i.1 x.1)
                      (set! j.1 x.1))
                  (if (true) (jump k.1) (jump l.1))))))))
    `(module ((new-frames ()) (locals ,locals))
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
                (jump x.1))
              (begin
                (if (= x.1 x.1)
                    (set! i.1 x.1)
                    (set! j.1 x.1))
                (if (true) (jump k.1) (jump l.1)))))))
    (equal? (list->set locals)
            (list->set '(a.1 b.1 c.1 d.1 e.1 f.1 g.1 h.1 i.1 j.1 k.1 l.1 v.1 x.1 y.1 z.1))))

  ;; aloc in pred
  (check-match
    (uncover-locals
      `(module ((new-frames ()))
        (begin
          (set! x.1 0)
          (if (not (not (begin (if (= a.1 b.1) (set! c.1 d.1) (set! e.1 f.1)) (> g.1 h.1))))
              (begin
                (if (not (<= i.1 j.1))
                    (set! x.1 2)
                    (set! x.1 3))
                (jump x.1))
              (begin
                (if (= k.1 l.1)
                    (set! x.1 4)
                    (set! x.1 5))
                (if (begin (!= m.1 n.1)) (jump r15) (jump r15)))))))
    `(module ((new-frames ()) (locals ,locals))
      (begin
        (set! x.1 0)
        (if (not (not (begin (if (= a.1 b.1) (set! c.1 d.1) (set! e.1 f.1)) (> g.1 h.1))))
            (begin
              (if (not (<= i.1 j.1))
                  (set! x.1 2)
                  (set! x.1 3))
              (jump x.1))
            (begin
              (if (= k.1 l.1)
                  (set! x.1 4)
                  (set! x.1 5))
              (if (begin (!= m.1 n.1)) (jump r15) (jump r15))))))
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
             (jump x.1))
           (begin
             (if (= k.1 l.1)
                 (set! x.1 4)
                 (set! x.1 5))
             (if (begin (!= m.1 n.1)) (jump r15) (jump r15))))))
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
               (jump x.1))
             (begin
               (if (= x.1 x.1)
                   (set! i.1 x.1)
                   (set! j.1 x.1))
               (if (true) (jump k.1) (jump l.1)))))))
  (check-match
    (uncover-locals
      `(module ((new-frames ()))
         (define L.foo.1 ((new-frames ())) ,m5-tail-1)
         (define L.bar.1 ((new-frames ())) ,m5-tail-2)
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
         ((new-frames ()))
         (define L.foo.1 ((new-frames ())) (jump r15))
         (define L.bar.1 ((new-frames ())) (jump r15))
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
       (define L.foo.1 ,foo-info (jump r15))
       (define L.bar.1 ,bar-info (jump r15))
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

;; Check return-point is handled correctly
(check-match
  (uncover-locals
    `(module ((new-frames ()))
        (define L.bar.1 ((new-frames ())) (begin (return-point L.barreturn.1 ,m5-tail-2) (jump r15)))
        ,m5-tail-1))
  `(module
      ,info
      (define L.bar.1 ,bar-info (begin (return-point L.barreturn.1 ,bar-tail) (jump r15)))
      ,tail)
  (and
    (equal? tail m5-tail-1)
    (equal? bar-tail m5-tail-2)
    (equal? (list->set (info-ref bar-info 'locals)) m5-locals-2)
    (equal? (list->set (info-ref info 'locals)) m5-locals-1)))

  ;; Check mops are handled correctly
  (check-match
  (uncover-locals
    `(module ((new-frames ()))
        (begin
          (mset! a.1 b.1 c.1)
          (set! f.1 (mref d.1 e.1))
          (jump f.1))))
  `(module
      ,info
      (begin
          (mset! a.1 b.1 c.1)
          (set! f.1 (mref d.1 e.1))
          (jump f.1)))
  (and
    (equal? (list->set (info-ref info 'locals)) (set 'a.1 'b.1 'c.1 'e.1 'd.1 'f.1))))
)

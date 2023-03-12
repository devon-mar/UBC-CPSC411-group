#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5
  "../utils/compiler-utils.rkt")

(provide replace-locations)

;; Milestone 2 Exercise 8
;; Milestone 4 Exercise 12
;; Milestone 5 Exercise 11
;;
;; Compiles Asm-Pred-lang /assignments to Nested-asm-lang, replaced each abstract
;; location with its assigned physical location from the assignment info field.
(define/contract (replace-locations p)
  (-> asm-pred-lang-v5/assignments? nested-asm-lang-v5?)

  ;; Returns true iff l is a rloc in Asm-Pred-Lang (or loc in Nested-Asm-Lang)
  ;; any -> boolean
  (define (rloc? l)
    (or (register? l) (fvar? l)))

  (define assignments?
    (listof (cons/c aloc? (listof rloc?))))

  ; Returns the assignment in a for aloc.
  (define/contract (assignment-ref a aloc)
    (-> assignments? aloc? rloc?)
    (car (dict-ref a aloc)))

  ;; Replaces each aloc in t with its assigned physical location from a
  ;;
  ;; a: assignments?
  ;; t: asm-pred-lang-v5-trg
  ;; -> nested-asm-lang-v5-trg 
  (define/contract (replace-locations-trg a t)
    (-> assignments? any/c any/c)
    (match t
      [(? label?) t]
      [_ (replace-locations-loc a t)]))

  ;; Replaces each aloc in l with its assigned physical location from a
  ;;
  ;; a: assignments?
  ;; l: asm-pred-lang-v5-loc
  ;; -> nested-asm-lang-v5-loc
  (define/contract (replace-locations-loc a l)
    (-> assignments? (or/c aloc? rloc?) rloc?)
    (match l
      [(? rloc?) l]
      [(? aloc?) (assignment-ref a l)]))

  ;; Replaces each aloc in t with its assigned physical location from a.
  ;;
  ;; a: assignments?
  ;; t: asm-pred-lang-v5/assignments-triv
  ;; -> nested-asm-lang-v5-triv
  (define/contract (replace-locations-triv a t)
    (-> assignments? any/c any/c)
    (match t
      [(? label?) t]
      [_ (replace-locations-opand a t)]))

  ;; Replaces each aloc in o with its assigned physical location from a.
  ;;
  ;; a: assignments?
  ;; o: asm-pred-lang-v5-opand
  ;; -> nested-asm-lang-v5-opand
  (define/contract (replace-locations-opand a o)
    (-> assignments? any/c any/c)
    (match o
      [(? int64?) o]
      [_ (replace-locations-loc a o)]))

  ;; Replaces each aloc in p with its assigned physical location from a.
  ;;
  ;; a: assignments?
  ;; p: asm-pred-lang-v5/assignments-pred
  ;; -> nested-asm-lang-v5-pred
  (define/contract (replace-locations-pred a p)
    (-> assignments? any/c any/c)
    (match p
      [`(true) p]
      [`(false) p]
      [`(not ,pred) `(not ,(replace-locations-pred a pred))]
      [`(begin ,effects ... ,pred)
       `(begin
         ,@(map (lambda (e) (replace-locations-effect a e)) effects)
         ,(replace-locations-pred a pred))]
      [`(if ,ppred ,pred1 ,pred2)
       `(if ,(replace-locations-pred a ppred)
            ,(replace-locations-pred a pred1)
            ,(replace-locations-pred a pred2))]
      [`(,relop ,loc ,opand)
       #:when(relop? relop)
       `(,relop ,(replace-locations-loc a loc) ,(replace-locations-opand a opand))]))

  ;; Replaces each aloc in e with its assigned physical location from a.
  ;;
  ;; a: assignments?
  ;; e: asm-pred-lang-v5/assignments-effect
  ;; -> nested-asm-lang-v5-effect
  (define/contract (replace-locations-effect a e)
    (-> assignments? any/c any/c)
    (match e
      [`(set! ,loc (,binop ,loc ,triv))
        `(set!
           ,(replace-locations-loc a loc)
           (,binop ,(replace-locations-loc a loc)
                   ,(replace-locations-triv a triv)))]
      [`(set! ,loc ,triv)
        `(set! ,(replace-locations-loc a loc) ,(replace-locations-triv a triv))]
      ;; removed tail `e` from template
      [`(begin ,es ...)
        `(begin
           ,@(map (lambda (e) (replace-locations-effect a e)) es))]
      [`(if ,pred ,effect1 ,effect2)
       `(if ,(replace-locations-pred a pred)
            ,(replace-locations-effect a effect1)
            ,(replace-locations-effect a effect2))]))

  ;; Replaces each aloc in t with its assigned physical location from a.
  ;;
  ;; a: assignments?
  ;; t: asm-pred-lang-v5/assignments-tail
  ;; -> nested-asm-lang-v5-tail
  (define/contract (replace-locations-tail a t)
    (-> assignments? any/c any/c)
    (match t
      [`(halt ,opand) `(halt ,(replace-locations-opand a opand))]
      [`(jump ,trg ,loc ...) `(jump ,(replace-locations-trg a trg))]
      [`(begin ,es ... ,tail)
        `(begin
           ,@(map (lambda (e) (replace-locations-effect a e)) es)
           ,(replace-locations-tail a tail))]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(replace-locations-pred a pred)
            ,(replace-locations-tail a tail1)
            ,(replace-locations-tail a tail2))]))

  ;; Replaces each aloc in tail with its assigned physical location
  ;; from the assignments in info.
  ;; proc ::= (define label tail)
  ;;
  ;; label info tail -> proc
  (define (replace-locations-proc label info tail)
    `(define ,label ,(replace-locations-tail (info-ref info 'assignment) tail)))

  ;; Replaces each aloc in t with its assigned physical location from the assignments
  ;; in p's info.
  ;; asm-pred-lang-v5/assignments-p -> nested-asm-lang-v5-p
  (define (replace-locations-p p)
    (match p
      [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
       `(module
         ,@(map replace-locations-proc labels infos tails)
         ,(replace-locations-tail (info-ref info 'assignment) tail))]))

  (replace-locations-p p))

(module+ test
  (require rackunit)

  (check-equal?
    (replace-locations
      '(module ((locals (x.1)) (assignment ((x.1 rax))))
        (begin
          (set! x.1 0)
          (halt x.1))))
    '(module (begin (set! rax 0) (halt rax))))
  (check-equal?
    (replace-locations
      '(module ((locals (x.1 y.1 w.1))
                (assignment ((x.1 rax) (y.1 rbx) (w.1 r9))))
        (begin
          (set! x.1 0)
          (set! y.1 x.1)
          (set! w.1 (+ w.1 y.1))
          (halt w.1))))
    '(module (begin (set! rax 0) (set! rbx rax) (set! r9 (+ r9 rbx)) (halt r9))))

  ;; nested begins
  (check-equal?
    (replace-locations
      '(module ((locals (x.1)) (assignment ((x.1 rax))))
        (begin
          (begin
            (begin
              (set! x.1 0)
              (set! x.1 42)))
          (halt x.1))))
    '(module
      (begin
        (begin
          (begin
            (set! rax 0)
            (set! rax 42)))
        (halt rax))))

  ;; replace in ifs 
  (check-equal?
    (replace-locations
      '(module
        ((locals (x.1 x.2 x.3 x.4))
         (assignment ((x.1 rdx) (x.2 r12) (x.3 fv0) (x.4 fv7))))
        (begin
          (set! x.1 5)
          (set! x.3 x.1)
          (if (= x.1 2)
              (set! x.2 9)
              (begin
                (set! x.4 1)
                (if (> x.3 x.2)
                    (set! x.3 (* x.3 x.2))
                    (set! x.2 (* x.2 x.3)))))
          (if (if (= x.3 x.2) (not (< x.3 2)) (>= x.4 9))
              (halt x.1)
              (halt x.4)))))
    '(module
      (begin
        (set! rdx 5)
        (set! fv0 rdx)
        (if (= rdx 2)
            (set! r12 9)
            (begin
              (set! fv7 1)
              (if (> fv0 r12)
                  (set! fv0 (* fv0 r12))
                  (set! r12 (* r12 fv0)))))
        (if (if (= fv0 r12) (not (< fv0 2)) (>= fv7 9))
            (halt rdx)
            (halt fv7)))))

  ;; replace in preds
  (check-equal?
    (replace-locations
      '(module
        ((locals (x.1 x.2 x.3 x.4 x.5))
         (assignment ((x.1 rdx) (x.2 r12) (x.3 fv0) (x.4 fv7) (x.5 fv5))))
        (begin
          (set! x.1 5)
          (set! x.3 x.1)
          (if (if (true) (= x.1 2) (not (not (!= x.3 9))))
              (set! x.2 9)
              (if (begin (> x.3 x.2))
                  (set! x.3 (* x.3 x.2))
                  (set! x.2 (* x.2 x.3))))
          (if (begin
                (begin (set! x.5 20))
                (set! x.2 x.4)
                (if (<= x.2 9) (set! x.4 9) (set! x.4 (+ x.4 x.4)))
                (if (false) (not (< x.3 2)) (false)))
              (halt x.1)
              (halt x.4)))))
    '(module
      (begin
        (set! rdx 5)
        (set! fv0 rdx)
        (if (if (true) (= rdx 2) (not (not (!= fv0 9))))
            (set! r12 9)
            (if (begin (> fv0 r12))
                (set! fv0 (* fv0 r12))
                (set! r12 (* r12 fv0))))
        (if (begin
              (begin (set! fv5 20))
              (set! r12 fv7)
              (if (<= r12 9) (set! fv7 9) (set! fv7 (+ fv7 fv7)))
              (if (false) (not (< fv0 2)) (false)))
            (halt rdx)
            (halt fv7)))))

  ;; rlocs
  (check-equal?
    (replace-locations
      '(module ((locals (x.1)) (assignment ((x.1 rax) (x.2 fv2))))
        (begin
          (set! x.1 0)
          (set! x.2 3)
          (set! rdx 10)
          (set! fv0 9)
          (set! rdx (+ rdx fv0))
          (set! fv0 (* fv0 x.2))
          (if (if (< rdx fv0) (>= fv0 x.2) (!= x.1 rdx))
              (halt rdx)
              (halt fv0)))))
    '(module
      (begin
        (set! rax 0)
        (set! fv2 3)
        (set! rdx 10)
        (set! fv0 9)
        (set! rdx (+ rdx fv0))
        (set! fv0 (* fv0 fv2))
        (if (if (< rdx fv0) (>= fv0 fv2) (!= rax rdx))
            (halt rdx)
            (halt fv0)))))

  ;; replace in jumps
  (check-equal?
    (replace-locations
      '(module ((locals (x.1 x.2)) (assignment ((x.1 rax) (x.2 fv0))))
        (define L.test.1 ((locals (x.2)) (assignment ((x.2 fv1))))
          (begin (set! x.2 rdx) (jump x.2 r11 fv3)))
        (define L.test.2 ((locals ()) (assignment ()))
          (jump done))
        (define L.test.3 ((locals ()) (assignment ()))
          (begin (set! r14 L.test.2) (jump r14)))
        (begin
          (set! x.1 L.test.1)
          (set! x.2 99)
          (set! rdx L.test.3)
          (jump x.1 x.2 rdx))))
    '(module
      (define L.test.1 (begin (set! fv1 rdx) (jump fv1)))
      (define L.test.2 (jump done))
      (define L.test.3 (begin (set! r14 L.test.2) (jump r14)))
      (begin
        (set! rax L.test.1)
        (set! fv0 99)
        (set! rdx L.test.3)
        (jump rax))))
  
  ;; replace in procedures
  (check-equal?
    (replace-locations
      '(module ((locals (x.1 x.2)) (assignment ((x.1 rax) (x.2 fv0))))
        (define L.test.1 ((locals (x.2)) (assignment ((x.2 fv1))))
          (begin (set! x.2 rdx) (jump x.2 r11)))
        (define L.test.2 ((locals (x.3 x.4)) (assignment ((x.3 rax) (x.4 fv0))))
          (if (not (begin (set! x.3 rdx) (not (< x.3 x.4)))) (halt x.3) (jump x.4)))
        (define L.test.3 ((locals (x.2 x.4)) (assignment ((x.2 r13) (x.4 fv1))))
          (begin (set! x.4 14) (set! x.2 rdx) (set! x.2 (+ x.2 x.4)) (jump x.4)))
        (begin
          (set! x.1 L.test.1)
          (set! x.2 99)
          (set! x.2 (* x.2 99))
          (set! rdx L.test.3)
          (jump x.1 x.2 rdx))))
    '(module
      (define L.test.1
        (begin (set! fv1 rdx) (jump fv1)))
      (define L.test.2
        (if (not (begin (set! rax rdx) (not (< rax fv0)))) (halt rax) (jump fv0)))
      (define L.test.3
        (begin (set! fv1 14) (set! r13 rdx) (set! r13 (+ r13 fv1)) (jump fv1)))
      (begin
        (set! rax L.test.1)
        (set! fv0 99)
        (set! fv0 (* fv0 99))
        (set! rdx L.test.3)
        (jump rax))))
  )

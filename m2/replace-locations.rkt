#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2)

(provide replace-locations)

;; Milestone 2 Exercise 8
;;
;; Compiles Asm-lang v2/assignments to Nested-asm-lang v2, replaced each abstract
;; location with its assigned physical location from the assignment info field.
(define/contract (replace-locations p)
  (-> asm-lang-v2/assignments? nested-asm-lang-v2?)

  (define assignments?
    (listof (cons/c aloc? any/c)))

  ; Returns the assignment in a for aloc.
  (define/contract (assignment-ref a aloc)
    (-> assignments? aloc? any/c)
    (car (dict-ref a aloc)))

  ;; Replaces each aloc in t with its assigned physical location from a.
  ;;
  ;; a: assignments?
  ;; t: asm-lang-v2/assignments-triv
  ;; -> nested-asm-lang-v2-triv
  (define/contract (replace-locations-triv a t)
    (-> assignments? any/c any/c)
    (match t
      [int64 #:when (int64? int64) int64]
      [aloc (assignment-ref a aloc)]))

  ;; Replaces each aloc in e with its assigned physical location from a.
  ;;
  ;; a: assignments?
  ;; e: asm-lang-v2/assignments-effect
  ;; -> nested-asm-lang-v2-effect
  (define/contract (replace-locations-effect a e)
    (-> assignments? any/c any/c)
    (match e
      [`(set! ,aloc (,binop ,aloc ,triv))
        `(set!
           ,(assignment-ref a aloc)
           (,binop ,(assignment-ref a aloc)
                   ,(replace-locations-triv a triv)))]
      [`(set! ,aloc ,triv)
        `(set! ,(assignment-ref a aloc) ,(replace-locations-triv a triv))]
      ;; removed tail `e` from template
      [`(begin ,es ...)
        `(begin
           ,@(map (lambda (e) (replace-locations-effect a e)) es))]))

  ;; Replaces each aloc in t with its assigned physical location from a.
  ;;
  ;; a: assignments?
  ;; t: asm-lang-v2/assignments-tail
  ;; -> nested-asm-lang-v2-tail
  (define/contract (replace-locations-tail a t)
    (-> assignments? any/c any/c)
    (match t
      [`(halt ,triv) `(halt ,(replace-locations-triv a triv))]
      [`(begin ,es ... ,tail)
        `(begin
           ,@(map (lambda (e) (replace-locations-effect a e)) es)
           ,(replace-locations-tail a tail))]))

  ;; Replaces each aloc in t with its assigned physical location from the assignments
  ;; in p's info.
  ;; asm-lang-v2/assignments-p -> nested-asm-lang-v2-p
  (define (replace-locations-p p)
    (match p
      [`(module ,info ,tail)
        (replace-locations-tail (info-ref info 'assignment) tail)]))

  (replace-locations-p p))

(module+ test
  (require rackunit)

  (check-equal?
    (replace-locations
      '(module ((locals (x.1)) (assignment ((x.1 rax))))
        (begin
          (set! x.1 0)
          (halt x.1))))
    '(begin (set! rax 0) (halt rax)))
  (check-equal?
    (replace-locations
      '(module ((locals (x.1 y.1 w.1))
                (assignment ((x.1 rax) (y.1 rbx) (w.1 r9))))
        (begin
          (set! x.1 0)
          (set! y.1 x.1)
          (set! w.1 (+ w.1 y.1))
          (halt w.1))))
    '(begin (set! rax 0) (set! rbx rax) (set! r9 (+ r9 rbx)) (halt r9)))

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
    '(begin
       (begin
         (begin
           (set! rax 0)
           (set! rax 42)))
       (halt rax))))

#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v2)

(provide assign-fvars)

;; Milestone 2 Exercise 7
;;
;; Compiles Asm-lang v2/locals to Asm-lang v2/assignments, by assigning each
;; abstract location from the locals info field to a fresh frame variable.
(define/contract (assign-fvars p)
  (-> asm-lang-v2/locals? asm-lang-v2/assignments?)

  ;; Maps each aloc in locals to a fresh fvar.
  (define/contract (assign-fvars-locals locals)
    ;; TODO figure out if I can remove the nested cons/c
    (-> (listof aloc?) (listof (cons/c aloc? (cons/c fvar? empty?))))
    (let-values
      ([(_ assignments)
        (for/fold
          ([index 0]
           [assignments '()])
          ([loc locals])
          (values (add1 index) (cons `(,loc ,(make-fvar index)) assignments)))])
      assignments))

  ;; Adds assignments to the info of p.
  ;;
  ;; asm-lang-v2/locals -> asm-lang-v2/assignments p
  (define (assign-fvars-p p)
    (match p
      [`(module ,info ,tail)
        `(module ,(info-set info 'assignment (assign-fvars-locals (info-ref info 'locals))) ,tail)]))

  (assign-fvars-p p))

(module+ test
  (require rackunit)

  ;; Returns #t if each assignment in locals has an assignment in a,
  ;; otherwise false.
  (define (check-assignment a locals)
    (andmap (lambda (l) (fvar? (car (dict-ref a l)))) locals))

  ; 1 assignment
  (check-match
    (assign-fvars 
     '(module
        ((locals (x.1)))
        (begin
          (set! x.1 0)
          (halt x.1))))
    `(module
       ,info
       (begin (set! x.1 0) (halt x.1)))
    (let
      ([locals (info-ref info 'locals)]
       [assignment (info-ref info 'assignment)])
      (and
        ; locals should not be changed
        (equal? locals '(x.1))
        (check-assignment assignment locals))))

  (check-match
    (assign-fvars
     '(module
        ((locals (x.1 y.1 w.1)))
        (begin
          (set! x.1 0)
          (set! y.1 x.1)
          (set! w.1 (+ w.1 y.1))
          (halt w.1))))
    `(module
       ,info
       (begin (set! x.1 0) (set! y.1 x.1) (set! w.1 (+ w.1 y.1)) (halt w.1)))
    (let
      ([locals (info-ref info 'locals)]
       [assignment (info-ref info 'assignment)])
      (and
        ; locals should not be changed
        (equal? locals '(x.1 y.1 w.1))
        (check-assignment assignment locals)))))


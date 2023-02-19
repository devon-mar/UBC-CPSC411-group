#lang racket

(require cpsc411/compiler-lib
         cpsc411/graph-lib)
(provide assign-registers)

;; Exercise #3
;; asm-lang-v2/conflicts -> asm-lang-v2/assignments
;; Assign register or frame location to each local variable
;; Assumes current-assignable-registers parameter to be set
(define (assign-registers p)
  (define fvar-counter 0)

  ;; (list aloc ...) (list (aloc (aloc ...))) -> (values aloc (list aloc ...) (list (aloc (aloc...))...))
  ;; removes low degree abstract location from aloc-list and return tuple of
  ;; abstract location, the aloc-list without the abstract location, and conf-list without the
  ;; abstract location
  (define (pop-ld-aloc aloc-list conf-list)
    (define sorted
      (sort aloc-list
            (lambda (a b)
              ;; Take first since the dict-ref returns singleton list
              (< (length (get-neighbors conf-list a)) (length (get-neighbors conf-list b))))))
    (define removed (car sorted))
    (values removed (remove removed aloc-list) (remove-vertex conf-list removed)))

  ;; aloc ((aloc loc)...) -> ((aloc loc) ...)
  ;; generates new aloc assignment by mapping aloc to an fvar
  (define (assign-aloc-to-fvar aloc prev-assignment)
    (define fvar
      (make-fvar (begin0 fvar-counter
                   (set! fvar-counter (+ 1 fvar-counter)))))
    (cons (list aloc fvar) prev-assignment))

  ;; aloc ((aloc loc)...) -> ((aloc loc) ...)
  ;; generates new aloc assignment by mapping aloc to a register in the whitelist
  (define (assign-aloc-to-reg aloc prev-assignment whitelist-regs)
    (cons (list aloc (first whitelist-regs)) prev-assignment))

  ;; aloc? (list aloc ...) (list (aloc (aloc...))...) ((aloc loc)...) -> ((aloc loc) ...)
  ;; assigns aloc to a location that does not conflict with existing assignments nor
  ;; its conflict list
  (define (assign-aloc aloc conf-list prev-assignment)
    ;; Take first since dict-ref returns singleton list of conflict list
    (define conflicts (get-neighbors conf-list aloc))
    (define blacklist-regs
      (for/fold ([reg-list '()]) ([c conflicts])
        ;; If conflict mapped to reg, get reg. Else empty list
        (define reg
          (if (register? (first (dict-ref prev-assignment c))) (dict-ref prev-assignment c) '()))
        (append reg-list reg)))
    (define whitelist-regs (set-subtract (current-assignable-registers) blacklist-regs))
    (if (empty? whitelist-regs)
        (assign-aloc-to-fvar aloc prev-assignment)
        (assign-aloc-to-reg aloc prev-assignment whitelist-regs)))

  ;; aloc-list conf-list -> aloc assignment
  ;; Takes list of abstract locations and list of conflicts, returns
  ;; register assignment for the abstract locations
  (define (generate-assignment aloc-list conf-list)
    (cond
      [(empty? aloc-list) `()]
      [else
       (define-values (pop-aloc new-aloc-list new-conf-list) (pop-ld-aloc aloc-list conf-list))
       (define new-assignment (generate-assignment new-aloc-list new-conf-list))
       (assign-aloc pop-aloc conf-list new-assignment)]))

  (define (convert-info info)
    (define local-list (info-ref info 'locals))
    (define conflicts (info-ref info 'conflicts))
    (info-set info 'assignment 
      (generate-assignment local-list conflicts)))

  (match p
    [`(module ,info ,tail
        )
     `(module ,(convert-info info) ,tail
        )]))

(module+ test
  (require
  rackunit
  cpsc411/langs/v2
   cpsc411/langs/v2-reg-alloc)

  (define t1
    '(module ((locals (x.1)) (conflicts ((x.1 ()))))
             (begin
               (set! x.1 42)
               (halt x.1))
       ))
  (define t2
    '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
              (conflicts ((x.3 (z.5 p.1 y.4 v.1 w.2)) (w.2 (z.5 p.1 y.4 v.1 x.3))
                                                      (v.1 (w.2 x.3))
                                                      (y.4 (t.6 z.5 p.1 w.2 x.3))
                                                      (p.1 (t.6 z.5 y.4 w.2 x.3))
                                                      (z.5 (t.6 p.1 y.4 w.2 x.3))
                                                      (t.6 (z.5 p.1 y.4)))))
             (begin
               (set! v.1 1)
               (set! w.2 46)
               (set! x.3 v.1)
               (set! p.1 7)
               (set! x.3 (+ x.3 p.1))
               (set! y.4 x.3)
               (set! p.1 4)
               (set! y.4 (+ y.4 p.1))
               (set! z.5 x.3)
               (set! z.5 (+ z.5 w.2))
               (set! t.6 y.4)
               (set! p.1 -1)
               (set! t.6 (* t.6 p.1))
               (set! z.5 (+ z.5 t.6))
               (halt z.5))
       ))

  (check-equal? (interp-asm-lang-v2/assignments (assign-registers t1))
                (interp-asm-lang-v2/conflicts t1))
  (check-equal? (interp-asm-lang-v2/assignments (parameterize ([current-assignable-registers '()]) (assign-registers t1)))
                (interp-asm-lang-v2/conflicts t1))
  (check-equal? (interp-asm-lang-v2/assignments (parameterize ([current-assignable-registers '(r9)]) (assign-registers t1)))
                (interp-asm-lang-v2/conflicts t1))
  (check-equal? (interp-asm-lang-v2/assignments (assign-registers t2))
                (interp-asm-lang-v2/conflicts t2))

  (check-equal? (assign-registers '(module ((locals (x.1)) (conflicts ((x.1 ()))))
                                           (begin
                                             (set! x.1 42)
                                             (halt x.1))
                                     ))
                '(module ((locals (x.1)) (conflicts ((x.1 ()))) (assignment ((x.1 r15))))
                         (begin
                           (set! x.1 42)
                           (halt x.1))
                   ))
  (check-equal? (parameterize ([current-assignable-registers '(r9)])
                  (assign-registers '(module ((locals (x.1)) (conflicts ((x.1 ()))))
                                             (begin
                                               (set! x.1 42)
                                               (halt x.1))
                                       )))
                '(module ((locals (x.1)) (conflicts ((x.1 ()))) (assignment ((x.1 r9))))
                         (begin
                           (set! x.1 42)
                           (halt x.1))
                   ))
  (check-equal? (parameterize ([current-assignable-registers '()])
                  (assign-registers '(module ((locals (x.1)) (conflicts ((x.1 ()))))
                                             (begin
                                               (set! x.1 42)
                                               (halt x.1))
                                       )))
                '(module ((locals (x.1)) (conflicts ((x.1 ()))) (assignment ((x.1 fv0))))
                         (begin
                           (set! x.1 42)
                           (halt x.1))
                   )))

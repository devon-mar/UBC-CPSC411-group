#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v5)

(provide assign-registers)

;; Milestone 3 Exercise #3
;; Milestone 4 Exercise 13
;; Milestone 5 Exercise 10
;;
;; Assign register or frame location to each local variable
;; Assumes current-assignable-registers parameter to be set
(define/contract (assign-registers p)
  (-> asm-pred-lang-v5/conflicts? asm-pred-lang-v5/assignments?)

  (define fvar-counter 0)

  ;; (list aloc ...) (list (aloc (loc ...))) -> (values aloc (list aloc ...) (list (aloc (aloc...))...))
  ;; removes low degree abstract location from aloc-list and return tuple of
  ;; abstract location, the aloc-list without the abstract location, and conf-list without the
  ;; abstract location
  (define (pop-ld-aloc aloc-list conf-list)
    (define sorted
      (sort aloc-list
            (lambda (a b)
              (< (length (get-neighbors conf-list a)) (length (get-neighbors conf-list b))))))
    (define removed (car sorted))
    (values removed (remove removed aloc-list) (remove-vertex conf-list removed)))

  ;; aloc ((aloc loc)...) (list fvar) -> ((aloc loc) ...)
  ;; generates new aloc assignment by mapping aloc to an fvar
  (define (assign-aloc-to-fvar aloc prev-assignment blacklist-fvars)
    (define fvar
      (make-fvar (begin0 fvar-counter
                   (set! fvar-counter (+ 1 fvar-counter)))))
    (if (set-member? blacklist-fvars fvar)
        (assign-aloc-to-fvar aloc prev-assignment blacklist-fvars)
        (cons (list aloc fvar) prev-assignment)))

  ;; aloc ((aloc loc)...) (list register) -> ((aloc loc) ...)
  ;; generates new aloc assignment by mapping aloc to a register in the whitelist
  (define (assign-aloc-to-reg aloc prev-assignment whitelist-regs)
    (cons (list aloc (first whitelist-regs)) prev-assignment))

  ;; aloc (list aloc ...) ((aloc loc)...) -> ((aloc loc) ...)
  ;; assigns aloc to a location that does not conflict with existing assignments nor
  ;; its conflict list
  (define (assign-aloc aloc conf-list prev-assignment)
    (define conflicts (get-neighbors conf-list aloc))
    (define-values (blacklist-regs blacklist-fvars)
      (for/fold ([reg-list '()] [fvar-list '()]) ([c conflicts])
        (match c
          [(? register?) (values (append reg-list (list c)) fvar-list)]
          [(? fvar?) (values reg-list (append fvar-list (list c)))]
          [(? aloc?)
           (define prev (first (dict-ref prev-assignment c)))
           (if (register? prev)
               (values (append (list prev) reg-list) fvar-list)
               ;; we don't need to add fvars from prev-assignment
               ;; since assignment fvars are always unique to the next assignment
               (values reg-list fvar-list))])))
    (define whitelist-regs (set-subtract (current-assignable-registers) blacklist-regs))
    (if (empty? whitelist-regs)
        (assign-aloc-to-fvar aloc prev-assignment blacklist-fvars)
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

  ;; info -> info
  ;; Takes info with locals & conflicts and returns info with assignments
  (define (convert-info info)
    (define local-list (info-ref info 'locals))
    (define conflicts (info-ref info 'conflicts))
    (info-set info 'assignment (generate-assignment local-list conflicts)))

  ;; label info tail -> proc
  ;; proc ::= (define label info tail)
  ;; Takes label info tail for proc and create proc with assignments in info
  (define (assign-registers-proc label info tail)
    `(define ,label ,(convert-info info) ,tail))

  (match p
    [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
     `(module
       ,(convert-info info)
       ,@(map assign-registers-proc labels infos tails)
       ,tail)]))

(module+ test
  (require
    rackunit
    cpsc411/langs/v5
    "../utils/gen-utils.rkt")

  (define t1
    '(module ((locals (x.1)) (conflicts ((x.1 ()))))
             (begin
               (set! x.1 42)
               (halt x.1))))
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
               (halt z.5))))

  (check-equal? (interp-asm-pred-lang-v5/assignments (assign-registers t1))
                (interp-asm-pred-lang-v5/conflicts t1))
  (check-equal? (interp-asm-pred-lang-v5/assignments
                 (parameterize ([current-assignable-registers '()]) (assign-registers t1)))
                (interp-asm-pred-lang-v5/conflicts t1))
  (check-equal? (interp-asm-pred-lang-v5/assignments
                 (parameterize ([current-assignable-registers '(r9)]) (assign-registers t1)))
                (interp-asm-pred-lang-v5/conflicts t1))
  (check-equal? (interp-asm-pred-lang-v5/assignments (assign-registers t2))
                (interp-asm-pred-lang-v5/conflicts t2))

  (check-equal? (assign-registers '(module ((locals (x.1)) (conflicts ((x.1 ()))))
                                           (begin
                                             (set! x.1 42)
                                             (halt x.1))))
                '(module ((locals (x.1)) (conflicts ((x.1 ()))) (assignment ((x.1 r15))))
                         (begin
                           (set! x.1 42)
                           (halt x.1))))
  (check-equal? (parameterize ([current-assignable-registers '(r9)])
                  (assign-registers '(module ((locals (x.1)) (conflicts ((x.1 ()))))
                                             (begin
                                               (set! x.1 42)
                                               (halt x.1)))))
                '(module ((locals (x.1)) (conflicts ((x.1 ()))) (assignment ((x.1 r9))))
                         (begin
                           (set! x.1 42)
                           (halt x.1))))
  (check-equal? (parameterize ([current-assignable-registers '()])
                  (assign-registers '(module ((locals (x.1)) (conflicts ((x.1 ()))))
                                             (begin
                                               (set! x.1 42)
                                               (halt x.1)))))
                '(module ((locals (x.1)) (conflicts ((x.1 ()))) (assignment ((x.1 fv0))))
                         (begin
                           (set! x.1 42)
                           (halt x.1))))

  ;; check does not assign to conflicting register
  (check-match
    (assign-registers
      '(module ((locals (x.1 x.2))
                (conflicts ((x.1 (fv0 r15 x.2)) (r15 (x.1 x.2)) (fv0 (x.1)) (x.2 (r15 x.1)))))
          (halt 0)))
    `(module ((locals (x.1 x.2))
              (conflicts ((x.1 (fv0 r15 x.2)) (r15 (x.1 x.2)) (fv0 (x.1)) (x.2 (r15 x.1))))
              (assignment ,assignment))
      (halt 0))
    (dict-equal? assignment '((x.2 r13) (x.1 r14))))

  ;; check does not assign to conflicting fvar
  (check-match
    (parameterize ([current-assignable-registers '()])
      (assign-registers
        '(module ((locals (x.1 x.2))
                  (conflicts ((x.1 (fv0 rdx)) (x.2 (fv2)) (rdx (x.1)) (fv0 (x.1)) (fv2 (x.2)))))
            (halt 2))))
    `(module ((locals (x.1 x.2))
              (conflicts ((x.1 (fv0 rdx)) (x.2 (fv2)) (rdx (x.1)) (fv0 (x.1)) (fv2 (x.2))))
              (assignment ,assignment))
      (halt 2))
    (dict-equal? assignment '((x.2 fv3) (x.1 fv1))))

  ;; assignment in procedures
  (check-match
    (assign-registers
      '(module ((locals (x.1 x.2))
                (conflicts ((x.1 (x.2 r15)) (x.2 (x.1)) (r15 (x.1)))))
          (define L.test.1 ((locals (x.2 x.3)) (conflicts ((x.2 ()) (x.3 ())))) (halt 1))
          (define L.test.2 ((locals (x.1 x.3)) (conflicts ((x.1 (x.3)) (x.3 (x.1))))) (halt 2))
          (define L.test.3 ((locals ()) (conflicts ())) (halt 3))
          (halt 0)))
    `(module ((locals (x.1 x.2))
              (conflicts ((x.1 (x.2 r15)) (x.2 (x.1)) (r15 (x.1))))
              (assignment ,assignment))
        (define L.test.1 ((locals (x.2 x.3))
                          (conflicts ((x.2 ()) (x.3 ())))
                          (assignment ,assignment1))
          (halt 1))
        (define L.test.2 ((locals (x.1 x.3))
                          (conflicts ((x.1 (x.3)) (x.3 (x.1))))
                          (assignment ,assignment2))
          (halt 2))
        (define L.test.3 ((locals ()) (conflicts ()) (assignment ,assignment3))
          (halt 3))
        (halt 0))
    (and (dict-equal? assignment '((x.1 r14) (x.2 r15)))
         (dict-equal? assignment1 '((x.2 r15) (x.3 r15)))
         (dict-equal? assignment2 '((x.1 r14) (x.3 r15)))))
  )

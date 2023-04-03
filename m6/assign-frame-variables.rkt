#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v7)

(provide assign-frame-variables)

;; Milestone 6 Exercise 13
;; Milestone 7 Exercise 7
;;
;; Assign frame location to each local variable
(define/contract (assign-frame-variables p)
  (-> asm-pred-lang-v7/spilled? asm-pred-lang-v7/assignments?)

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
  (define (assign-aloc-to-fvar aloc prev-assignment blacklist-fvars fvar-counter)
    (define fvar (make-fvar fvar-counter))
    (if (set-member? blacklist-fvars fvar)
        (assign-aloc-to-fvar aloc prev-assignment blacklist-fvars (+ 1 fvar-counter))
        (cons (list aloc fvar) prev-assignment)))

  ;; aloc (list aloc ...) ((aloc loc)...) -> ((aloc loc) ...)
  ;; assigns aloc to a location that does not conflict with existing assignments nor
  ;; its conflict list
  (define (assign-aloc aloc conf-list prev-assignment)
    (define conflicts (get-neighbors conf-list aloc))
    (define blacklist-fvars
      (for/fold ([fvar-list '()]) ([c conflicts])
        (match c
          [(? register?) fvar-list]
          [(? fvar?) (append fvar-list (list c))]
          [(? aloc?)
           (define prev 
              (if
                (dict-has-key? prev-assignment c)
                (first (dict-ref prev-assignment c))
                (void)))
           (if (register? prev)
               fvar-list
               (append fvar-list (list prev)))])))
      ;; Begin fvar search at 0 to use minimum fvar offset possible
      (assign-aloc-to-fvar aloc prev-assignment blacklist-fvars 0))

  ;; aloc-list conf-list aloc-assignment -> aloc-assignment
  ;; Takes list of abstract locations, list of conflicts and list of assignments that already
  ;; exist in info. Returns register assignment for the abstract locations
  (define (generate-assignment aloc-list conf-list info-assignment)
    (cond
      [(empty? aloc-list) info-assignment]
      [else
       (define-values (pop-aloc new-aloc-list new-conf-list) (pop-ld-aloc aloc-list conf-list))
       (define new-assignment (generate-assignment new-aloc-list new-conf-list info-assignment))
       (assign-aloc pop-aloc conf-list new-assignment)]))

  ;; info -> info
  ;; Takes info with locals & conflicts and returns info with assignments
  (define (convert-info info)
    (define local-list (info-ref info 'locals))
    (define conflicts (info-ref info 'conflicts))
    (define new-assignment (generate-assignment local-list conflicts (info-ref info 'assignment)))
    (begin
      ;; Reset fvar counter when in new scope since conflict graph and locals are
      ;; independent from all other scopes.
      (info-remove
        (info-remove
          (info-set info 'assignment new-assignment)
          'conflicts)
        'locals )))

  ;; label info tail -> proc
  ;; proc ::= (define label info tail)
  ;; Takes label info tail for proc and create proc with assignments in info
  (define (assign-frame-variables-proc label info tail)
    `(define ,label ,(convert-info info) ,tail))

  (match p
    [`(module ,info (define ,labels ,infos ,tails) ... ,tail)
     `(module
       ,(convert-info info)
       ,@(map assign-frame-variables-proc labels infos tails)
       ,tail)]))

(module+ test
  (require
    rackunit
    cpsc411/langs/v7
    "../utils/gen-utils.rkt")

  (define t2
    '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
              (conflicts ((x.3 (z.5 p.1 y.4 v.1 w.2)) (w.2 (z.5 p.1 y.4 v.1 x.3))
                                                      (v.1 (w.2 x.3))
                                                      (y.4 (t.6 z.5 p.1 w.2 x.3))
                                                      (p.1 (t.6 z.5 y.4 w.2 x.3))
                                                      (z.5 (t.6 p.1 y.4 w.2 x.3))
                                                      (t.6 (z.5 p.1 y.4))))
              (assignment ()))
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
               (jump r15))))

  (check-match
    (assign-frame-variables t2)
    `(module
      ((assignment
        ,assignments))
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
        (jump r15)))
    (equal? (list->set (map first assignments)) (list->set (map first `((p.1 fv0) (t.6 fv1) (z.5 fv2) (y.4 fv3) (x.3 fv1) (w.2 fv4) (v.1 fv0))))))

  (check-equal? (assign-frame-variables '(module 
                                          ((locals (x.1))
                                           (conflicts ((x.1 ())))
                                           (assignment ()))
                                           (begin
                                             (set! x.1 42)
                                             (jump r15))))
                '(module
                  ((assignment ((x.1 fv0))))
                    (begin
                      (set! x.1 42)
                      (jump r15))))

  ;; check does not assign to conflicting register
  (check-match
    (assign-frame-variables
      '(module 
          ((locals (x.1 x.2))
           (conflicts ((x.1 (fv0 r15 x.2)) (r15 (x.1 x.2)) (fv0 (x.1)) (x.2 (r15 x.1))))
           (assignment ()))
                (begin
                  (set! x.1 42)
                  (jump r15))))
    `(module ((assignment ,assignment))
            (begin
              (set! x.1 42)
              (jump r15)))
    (dict-equal? assignment '((x.2 fv0) (x.1 fv1))))


  ;; check does not assign to already assigned frame veriable in assignments
  (check-match
    (assign-frame-variables
      '(module 
          ((locals (x.1 x.2))
           (conflicts ((x.1 (fv0 r15 x.2 x.3)) (r15 (x.1 x.2)) (fv0 (x.1)) (x.2 (r15 x.1 x.3)) (x.3 (x.1 x.2))))
           (assignment ((x.3 fv0))))
                (begin
                  (set! x.1 42)
                  (jump r15))))
    `(module ((assignment ,assignment))
            (begin
              (set! x.1 42)
              (jump r15)))
    (dict-equal? assignment '((x.2 fv2) (x.1 fv1) (x.3 fv0))))

  ;; check that it preserves existing register assignments
  (check-match
    (assign-frame-variables
      '(module 
          ((locals (x.1 x.2))
           (conflicts ((x.1 (fv0 r15 x.2 x.3)) (r15 (x.1 x.2)) (fv0 (x.1)) (x.2 (r15 x.1 x.3)) (x.3 (x.1 x.2))))
           (assignment ((x.3 rax))))
                (begin
                  (set! x.1 42)
                  (jump r15))))
    `(module ((assignment ,assignment))
            (begin
              (set! x.1 42)
              (jump r15)))
    (dict-equal? assignment '((x.2 fv0) (x.1 fv1) (x.3 rax))))

  ;; assignment in procedures
  (check-match
    (assign-frame-variables
      '(module ((locals (x.1 x.2))
                (conflicts ((x.1 (x.2 r15)) (x.2 (x.1)) (r15 (x.1))))
                (assignment ()))
          (define L.test.1 ((locals (x.2 x.3)) (conflicts ((x.2 ()) (x.3 ()))) (assignment ())) (jump r15))
          (define L.test.2 ((locals (x.1 x.3)) (conflicts ((x.1 (x.3)) (x.3 (x.1)))) (assignment ())) (jump r15))
          (define L.test.3 ((locals ()) (conflicts ()) (assignment ())) (jump r15))
          (jump r15)))
    `(module 
        ((assignment ,assignment))
        (define L.test.1 ((assignment ,assignment1))
                        (jump r15))
        (define L.test.2 ((assignment ,assignment2))
                          (jump r15))
        (define L.test.3 ((assignment ,assignment3)) (jump r15))
        (jump r15))
    (and (dict-equal? assignment '((x.2 fv1) (x.1 fv0)))
         (dict-equal? assignment1 '((x.3 fv0) (x.2 fv0)))
         (dict-equal? assignment2 '((x.3 fv0) (x.1 fv1)))
         (dict-equal? assignment3 '()))))

#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

(provide inline-jumps)

;; Milestone 4 Exercise 7
;; Remove jump instructions that go directly to the next instruction
;; from a Para-Asm-Lang program
(define/contract (inline-jumps p)
  (-> para-asm-lang-v4? para-asm-lang-v4?)

  ;; (Para-Asm-Lang-v4 s) -> (List-of (Para-Asm-Lang-v4 label))
  ;; Return all labels associated with 's'
  (define (get-labels s)
    (match s
      [`(with-label ,label ,si)
       (append (list label) (get-labels si))]
      ;; For:
      ;; (halt opand)
 	 	  ;; (set! loc triv)
 	 	  ;; (set! loc_1 (binop loc_1 opand))
      ;; (jump trg)
 	 	  ;; (compare loc opand)
 	 	  ;; (jump-if relop trg)
      [_ '()]))
  
  ;; (Para-Asm-Lang-v4 s) (List-of (Para-Asm-Lang-v4 label)) -> boolean
  ;; Given 's' and labels for the next 's',
  ;; return true if 's' is a jump that can be removed, false otherwise
  (define (remove-jump s nextlabels)
    (match s
      [`(jump ,trg) (and (label? trg) (member trg nextlabels))]
      ;; For:
      ;; (halt opand)
 	 	  ;; (set! loc triv)
 	 	  ;; (set! loc_1 (binop loc_1 opand))
 	 	  ;; (with-label label s)
 	 	  ;; (compare loc opand)
 	 	  ;; (jump-if relop trg)
      [_ #f]))

  (match p
    [`(begin ,ss ...)
     (define-values (new-s _)
       (for/foldr ([new-s '()] [nextlabels '()]) ([s ss])
         (if (remove-jump s nextlabels)
             (values new-s nextlabels)
             (values (append (list s) new-s) (get-labels s)))))
     `(begin ,@new-s)]))


(module+ test
  (require rackunit)
  (check-equal? (inline-jumps '(begin)) '(begin))

  ;; remove jump
  (check-equal?
    (inline-jumps
      '(begin
        (set! r9 10)
        (jump L.test.1)
        (with-label L.test.1 (halt r9))))
    '(begin
      (set! r9 10)
      (with-label L.test.1 (halt r9))))
  
  ;; remove nested jump
  (check-equal?
    (inline-jumps
      '(begin
        (set! fv0 99)
        (jump L.test.1)
        (with-label L.test.2 (with-label L.test.1 (halt fv0)))))
    '(begin
      (set! fv0 99)
      (with-label L.test.2 (with-label L.test.1 (halt fv0)))))
  
  ;; remove all consecutive jumps
  (check-equal?
    (inline-jumps
      '(begin
        (set! r9 10)
        (jump L.test.1)
        (jump L.test.2)
        (jump L.test.1)
        (jump L.test.3)
        (jump L.test.1)
        (jump L.test.1)
        (with-label L.test.1 (with-label L.test.3 (halt r9)))
        (with-label L.test.2 (halt 10))))
    '(begin
      (set! r9 10)
      (jump L.test.1)
      (jump L.test.2)
      (with-label L.test.1 (with-label L.test.3 (halt r9)))
      (with-label L.test.2 (halt 10))))
  
  ;; preserves jump with label
  (check-equal?
    (inline-jumps
      '(begin
        (set! r13 10)
        (with-label L.test.2 (jump L.test.1))
        (with-label L.test.1 (halt r13))))
    '(begin
      (set! r13 10)
      (with-label L.test.2 (jump L.test.1))
      (with-label L.test.1 (halt r13))))

  ;; preserves jump-if
  (check-equal?
    (inline-jumps
      '(begin
        (set! rdx 10)
        (compare rdx 9)
        (jump-if = L.test.1)
        (with-label L.test.1 (halt rdx))))
    '(begin
      (set! rdx 10)
      (compare rdx 9)
      (jump-if = L.test.1)
      (with-label L.test.1 (halt rdx))))
  
  ;; preserves jump with instructions between
  (check-equal?
    (inline-jumps
      '(begin
        (set! r9 10)
        (jump L.test.1)
        (jump L.test.2)
        (with-label L.test.1 (set! r9 (* r9 10)))
        (jump L.test.2)
        (set! r9 (+ r9 5))
        (with-label L.test.2 (halt r9))))
    '(begin
      (set! r9 10)
      (jump L.test.1)
      (jump L.test.2)
      (with-label L.test.1 (set! r9 (* r9 10)))
      (jump L.test.2)
      (set! r9 (+ r9 5))
      (with-label L.test.2 (halt r9))))
  )
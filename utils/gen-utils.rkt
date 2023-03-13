#lang racket

(provide
  append-e
  list-equiv?
  dict-equal?)

;; General util functions

;; List, Any -> List
;; Append element to end of list
(define (append-e l e)
  (append l (list e)))

;; list list -> boolean
;; Check if list contains the same elements
(define (list-equiv? list1 list2)
  (empty? (set-symmetric-difference list1 list2)))

;; Dict(K V) Dict(K V) ((V V) -> boolean) -> boolean
;; Check if two dictionaries contain the same data
(define (dict-equal? dict1 dict2 [val-equal? equal?])
  (and (= (dict-count dict1) (dict-count dict2))
       (list-equiv? (dict-keys dict1) (dict-keys dict2))
       (for/and ([k (dict-keys dict1)])
         (val-equal? (dict-ref dict1 k) (dict-ref dict2 k)))))

(module+ test
  (require rackunit)
  (check-equal? (append-e '() 1) '(1))
  (check-equal? (append-e '(a b c) 'd) '(a b c d))
  (check-equal? (append-e '(a b c) '(d e f)) '(a b c (d e f)))

  (check-true (list-equiv? '() '()))
  (check-false (list-equiv? '() '(1)))
  (check-false (list-equiv? '(2) '(1)))
  (check-true (list-equiv? '(1 2 3) '(2 3 1)))
  (check-true (list-equiv? '(1 (1 2) 3) '((1 2) 3 1)))
  (check-false (list-equiv? '(1 (2 1) 3) '((1 2) 3 1)))

  (check-true (dict-equal? '() '()))
  (check-true (dict-equal? '((a 1)) '((a 1))))
  (check-false (dict-equal? '((b 2 3) (c 4)) '((c 4) (a 1) (b 2 3))))
  (check-false (dict-equal? '((b 3 2) (c 4) (a 1)) '((c 4) (a 1) (b 2 3))))
  (check-true (dict-equal? '((b 2 3) (c 4) (a 1)) '((c 4) (a 1) (b 2 3))))

  (check-true (dict-equal? '((b 3 2 1) (c 4 5) (a 1)) '((c 5 4) (a 1) (b 2 1 3)) list-equiv?))
  )

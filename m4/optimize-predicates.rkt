#lang racket

(require cpsc411/compiler-lib
         cpsc411/graph-lib)
(provide assign-registers)

;; Exercise #3
;; nested-asm-lang-v4? -> nested-asm-lang-v4?
(define (optimize-predicates p)
  (define env (make-hash))

  (define (convert-tail t)
    (match t
      [`(halt ,triv)
        `(halt ,triv)]
      [`(begin ,effects ... ,tail)
        `(begin ,@effects ,tail)]
      [`(if ,pred ,t1 ,t2)
        (convert-if pred t1 t2)]))
  
  (define (convert-if pred t1 t2)
    (match pred
      [`(,relop ,loc ,triv)
        ]
      [`(true)
        ]
      [`(false)
        ]
      [`(not ,nested-pred)
        ]
      [`(begin ,effects ... ,pred)
        ]
      [`(if ,pred )]))
  (define (convert-effect e))
  (match p
    [`(module ,tail)
      ]))

(module+ test
  (require rackunit)
  (define in1 `(module (begin (set! reg 1) (> reg 0))))
  (define out1 `(begin (set! reg 1) (true)))
  
  (define in2 `(module (begin (set! reg 1) (< reg 0))))
  (define out2 `(module (begin (set! reg 1) (false))))

  (define in3 `(module (begin (set! reg opand_1) (< reg ,(max-int 64)))))
  (define out3 `(module (begin (set! reg opand_1) (true))))

  (define in4 `(module (begin (set! reg opand_1) (= reg opand_1))))
  (define out4 `(module (begin (set! reg opand_1) (true))))

  (define in5 `(module (begin (set! reg int64_1) (= reg int64_2))))
  (define out5 `(module (false)))

  (check-equal? (optimize-predicates in1) out1)
  (check-equal? (optimize-predicates in2) out2)
  (check-equal? (optimize-predicates in3) out3)
  (check-equal? (optimize-predicates in4) out4)
  (check-equal? (optimize-predicates in5) out5)

)

#lang racket

(require cpsc411/compiler-lib
         cpsc411/graph-lib)
(provide optimize-predicates)

;; Exercise #3
;; nested-asm-lang-v4? -> nested-asm-lang-v4?
(define (optimize-predicates p)
  (define env (make-hash))

  ;; relop -> procedure
  (define (symbol->relop relop)
    (match relop
      [`>= >=]
      [`<= <=]
      [`< <]
      [`> >]
      [`= =]
      [`!= (lambda (x y) (not (= x y)))]))

  ;; binop -> procedure
  (define (symbol->binop binop)
    (match binop
      [`+ +]
      [`* *]))
  
  ;; triv -> triv
  (define (convert-triv triv)
    (match triv
      [(? integer?) triv]
      [(? symbol?) 
        (if 
          (dict-has-key? env triv)
          (dict-ref env triv)
          triv)]))
  
  ;; tail -> tail
  (define (convert-tail t)
    (match t
      [`(halt ,triv)
        `(halt ,triv)]
      [`(begin ,effects ... ,tail)
        `(begin ,@(map convert-effect effects) ,(convert-tail tail))]
      [`(if ,pred ,tail1 ,tail2)
        (convert-if pred (convert-tail tail1) (convert-tail tail2))]))
  
  ;; pred tail tail -> tail
  ;; pred effect effect -> effect
  (define (convert-if pred t1 t2)
    (match pred
      [`(,relop ,loc ,triv)
        (convert-relop 
          relop
          (convert-triv loc)
          (convert-triv triv)
          t1
          t2)]
      [`(true)
        t1]
      [`(false)
        t2]
      [`(not ,nested-pred)
        (convert-if nested-pred t2 t1)]
      [`(begin ,effects ... ,nested-pred)
        `(begin ,@(map convert-effect effects) ,(convert-if nested-pred t1 t2))]
      [`(if ,pred ,pred1 ,pred2)
        (convert-if pred
          (convert-if pred1 t1 t2)
          (convert-if pred2 t1 t2))]))

  ;; relop triv triv tail tail -> tail
  ;; relop triv triv effect effect -> effect
  (define (convert-relop relop a1 a2 tail1 tail2)
    (define (abstract-relop relop)
    (if (and (integer? a1)
            (integer? a2))
        (if ((symbol->relop relop) a1 a2)
          tail1
          tail2)
        `(if (,relop ,a1 ,a2) ,tail1 ,tail2)))
    (abstract-relop relop))

  ;; effect -> effect
  (define (convert-effect e)
    (match e
      [`(set! ,loc_1 (,binop ,loc_1 ,opand))
        (begin0 `(set! ,loc_1 (,binop ,loc_1 ,opand))
          (dict-set! env loc_1 (convert-binop binop loc_1 opand)))]
      [`(set! ,loc ,triv)
        (begin0 `(set! ,loc ,triv)
          (dict-set! env loc (convert-triv triv)))]
      [`(begin ,effects ...)
        `(begin ,@(map convert-effect effects))]
      [`(if ,pred ,effect1 ,effect2)
        (convert-if pred (convert-effect effect1) (convert-effect effect2))]))

  ;; binop loc triv -> symbol | integer
  (define (convert-binop binop loc opand)
    (define interp-loc (convert-triv loc))
    (define interp-opand (convert-triv opand))
    (if 
      (and (integer? interp-loc) (integer? interp-opand))
      ((symbol->binop binop) interp-loc interp-opand)
      (`(,binop ,loc ,opand))))

  (match p
    [`(module ,tail)
      `(module ,(convert-tail tail))]))

(module+ test
  (require rackunit)
  (define in1 `(module (begin (set! fv1 1) (if (> fv1 0) (halt 1) (halt 2)))))
  (define out1 `(module (begin (set! fv1 1) (halt 1))))
  
  (define in2 `(module (begin (set! fv1 1) (if (< fv1 0) (halt 1) (halt 2)))))
  (define out2 `(module (begin (set! fv1 1) (halt 2))))

  (define in3 `(module (begin (set! fv1 1) (if (< fv1 ,(max-int 64)) (halt 1) (halt 2)))))
  (define out3 `(module (begin (set! fv1 1) (halt 1))))

  (define in4 `(module (begin (set! fv1 1) (if (= fv1 1) (halt 1) (halt 2)))))
  (define out4 `(module (begin (set! fv1 1) (halt 1))))

  (define in5 `(module (begin (set! fv1 int64_1) (if (= fv1 int64_2) (halt 1) (halt 2)))))
  (define out5 `(module (halt 2)))

  (check-equal? (optimize-predicates in1) out1)
  (check-equal? (optimize-predicates in2) out2)
  (check-equal? (optimize-predicates in3) out3)
  (check-equal? (optimize-predicates in4) out4)
  (check-equal? (optimize-predicates in5) out5)

)

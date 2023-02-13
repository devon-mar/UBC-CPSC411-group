#lang racket
(require 
    cpsc411/compiler-lib
    "compiler.rkt")
;; asm-lang-v2 -> nested-asm-lang-v2
;; Replaces each abstract location with a physical location
(define (assign-homes-opt p)
    (define (set-triv triv mapping)
        (match triv
            [(? integer?) triv]
            [_ (first (dict-ref mapping triv))]))
    (define (set-effect p mapping)
        (match p
            [`(set! ,triv1 (,binop ,triv1 ,triv2))
                (define new-triv1 (set-triv triv1 mapping))
                (define new-triv2 (set-triv triv2 mapping))
                `(set! ,new-triv1 (,binop ,new-triv1 ,new-triv2))]
            [`(set! ,triv1 ,triv2)
                (define new-triv1 (set-triv triv1 mapping))
                (define new-triv2 (set-triv triv2 mapping))
                `(set! ,new-triv1 , new-triv2)]
            [`(begin ,effect ...)
                `(begin ,@(map (lambda (e) (set-effect e mapping)) effect))]))
    (define (set-tail p mapping)
        (match p
            [`(halt ,triv)
                `(halt ,(set-triv triv mapping))]
            [`(begin ,effect ... ,tail)
                `(begin ,@(map (lambda (e) (set-effect e mapping)) effect) ,(set-tail tail mapping))]))
    (define (assign-homes p) 
        (match p
            [`(module (,info ... (assignment (,mapping ...))) ,tail)
                `(module (,@info (assignment ,mapping)) ,(set-tail tail mapping))]))
    (assign-homes
        (assign-registers 
            (conflict-analysis 
                (undead-analysis p)))))
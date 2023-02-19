#lang racket

;; Compare the compile and running time of M2 vs. M3
(module+ test
  (require
    rackunit
    cpsc411/compiler-lib
    "compile-m2.rkt"
    "compile-m3.rkt")

  ;; int -> Asm-lang-v2
  ;; Create a long Asm-lang program with `i` nested lets
  (define (gen-long-program i)
    (define (gen-tail i)
      (if (<= i 0)
          'a
          `(let ([a (+ a 2)] [b (+ b 3)] [c (* c 2)])
             ,(gen-tail (- i 1)))))
    `(module (let ([a 7] [b 8] [c 1]) ,(gen-tail (- i 1)))))
  
  ;; (Asm-lang-v2 -> x64) Asm-lang-v2 -> int
  ;; Times the compiler's compile time and the running time of execution
  ;; Return the result of the execution
  (define (test-compiler compiler p)
    (println "compile:")
    (define c (time (compiler p)))
    (println "execution:")
    (define v (time (nasm-run/read c)))
    (println (format "x64 program length: ~a" (string-length c)))
    v)

  (define p1
    '(module
      (let ([x (let ([y 3]) (* y 2))]
            [a (let ([y 3] [x 10]) (+ x y))]
            [b 4]
            [d 99])
        (let ([z (+ x 9)]
              [a (* a b)]
              [c (+ b 7)])
          (let ([z (* z a)]
                [t (let ([i (let () 14)]) i)])
            (* x z))))))
  
  (define p2 (gen-long-program 100))
  
  (println "compile-m2: program 1")
  (define m2-p1 (test-compiler compile-m2 p1))
  (println "compile-m3: program 1")
  (define m3-p1 (test-compiler compile-m3 p1))
  (println "compile-m2: program 2")
  (define m2-p2 (test-compiler compile-m2 p2))
  (println "compile-m3: program 2")
  (define m3-p2 (test-compiler compile-m3 p2))

  (check-equal? m2-p1 m3-p1)
  (check-equal? m2-p2 m3-p2)
  )
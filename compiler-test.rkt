#lang racket

(module+ test
  (require rackunit cpsc411/compiler-lib cpsc411/2c-run-time "compiler.rkt")

  ; generate-x64 tests
  (check-equal? (generate-x64 '(begin)) "\n")
  (check-equal?
    (generate-x64 '(begin (set! rax 42)))
    "mov rax, 42\n")
  (check-equal?
    (generate-x64 '(begin
      (set! (rbp - 0) 0)
      (set! (rbp - 8) rcx)
      (set! rax (rbp - 24))
      (set! rdx rbp)
      (set! r14 (+ r14 rdx))
      (set! rax (+ rax (rbp - 8)))
      (set! rax (* rax (rbp - 16)))))
    (string-join
      '("mov QWORD [rbp - 0], 0\n"
        "mov QWORD [rbp - 8], rcx\n"
        "mov rax, QWORD [rbp - 24]\n"
        "mov rdx, rbp\n"
        "add r14, rdx\n"
        "add rax, QWORD [rbp - 8]\n"
        "imul rax, QWORD [rbp - 16]\n") ""))
  
  ; implement-fvars tests
  (check-equal? (implement-fvars '(begin)) '(begin))
  (check-equal?
    (implement-fvars
      '(begin
        (set! rax 42)
        (set! rbx rax)
        (set! rbx (+ rbx 20))
        (set! rcx (* rcx rbx))))
    '(begin
      (set! rax 42)
      (set! rbx rax)
      (set! rbx (+ rbx 20))
      (set! rcx (* rcx rbx))))
  (check-equal?
    (implement-fvars
      '(begin
        (set! fv0 42)
        (set! fv4 rax)
        (set! rbx fv10)
        (set! rcx (* rcx fv6))))
    '(begin
      (set! (rbp - 0) 42)
      (set! (rbp - 32) rax)
      (set! rbx (rbp - 80))
      (set! rcx (* rcx (rbp - 48)))))
  
  ; patch-instructions tests
  (check-equal? (patch-instructions '(begin (halt 0))) '(begin (set! rax 0)))
  (check-equal?
    (patch-instructions
      `(begin
        (set! rax 0)
        (set! rbx 42)
        (set! rcx (+ rcx 9))
        (set! rdx (* rbx rcx))
        (halt rdx)))
    `(begin
      (set! rax 0)
      (set! rbx 42)
      (set! rcx (+ rcx 9))
      (set! rdx (* rbx rcx))
      (set! rax rdx)))
  (check-equal?
    (patch-instructions
      `(begin
        (set! fv0 0)
        (set! fv1 42)
        (set! fv0 fv1)
        (halt fv0)))
    '(begin
      (set! fv0 0)
      (set! fv1 42)
      (set! r10 fv1)
      (set! fv0 r10)
      (set! rax fv0)))
  (check-equal?
    (patch-instructions
      `(begin
        (set! fv0 ,(max-int 64))         ; fvar w/ int64
        (set! fv2 (+ fv2 ,(max-int 64))) ; binop fvar w/ int64
        (set! fv9 (+ fv9 ,(max-int 32))) ; binop fvar w/ int32
        (set! fv5 (* fv5 rax))           ; binop fvar w/ reg
        (halt fv2)))
    `(begin
      (set! r10 ,(max-int 64)) ; fvar w/ int64 splits in two
      (set! fv0 r10)
      (set! r10 fv2)           ; binop fvar w/ int64 splits in four
      (set! r11 ,(max-int 64))
      (set! r10 (+ r10 r11))
      (set! fv2 r10)
      (set! r10 fv9)           ; binop fvar w/ int32 splits in three
      (set! r10 (+ r10 ,(max-int 32)))
      (set! fv9 r10)
      (set! r10 fv5)           ; binop fvar w/ reg splits in three
      (set! r10 (* r10 rax))
      (set! fv5 r10)
      (set! rax fv2)))
  
  ; flatten-begins tests
  (check-equal? (flatten-begins '(halt 0)) '(begin (halt 0)))
  (check-equal? (flatten-begins '(begin (halt 0))) '(begin (halt 0)))
  (check-equal?
    (flatten-begins '(begin (set! rax 0) (set! rbx (+ rbx 20)) (set! fv3 10) (halt 0)))
    '(begin (set! rax 0) (set! rbx (+ rbx 20)) (set! fv3 10) (halt 0)))
  (check-equal?
    (flatten-begins
      '(begin (begin (begin (begin (set! rax 4) (halt rax))))))
    '(begin (set! rax 4) (halt rax)))
  (check-equal?
    (flatten-begins
      '(begin
        (begin (set! rbx 10) (begin (begin (set! rbx (* rbx 20))) (set! fv10 10)) (set! fv10 rbx))
        (set! rax 4)
        (set! rdx (+ rdx 10))
        (begin (set! rdx (* rdx rdx)))
        (begin (set! rbx 2) (set! rax (+ rax fv1)) (begin (set! rcx 4) (halt rdx)))))
    '(begin
      (set! rbx 10)
      (set! rbx (* rbx 20))
      (set! fv10 10)
      (set! fv10 rbx)
      (set! rax 4)
      (set! rdx (+ rdx 10))
      (set! rdx (* rdx rdx))
      (set! rbx 2)
      (set! rax (+ rax fv1))
      (set! rcx 4)
      (halt rdx)))
  
  ; uncover-locals tests
  (check-equal? (uncover-locals '(module () (halt 0))) '(module ((locals ())) (halt 0)))
  (check-equal?
    (uncover-locals
      '(module ()
        (begin
          (begin (set! x.1 1) (halt x.1)))))
    '(module ((locals (x.1)))
      (begin
        (begin (set! x.1 1) (halt x.1)))))
  (check-equal?
    (uncover-locals
      '(module ((other (1 2 7 7)) (other4 "hi"))
        (halt tmp.1)))
    '(module ((other (1 2 7 7)) (other4 "hi") (locals (tmp.1)))
      (halt tmp.1)))
  (check-match
    (uncover-locals
      '(module ()
        (begin
          (set! x.1 0)
          (set! y.1 x.1)
          (begin (set! a.3 2) (begin (set! b.2 (* b.2 h.9))) (set! a.3 9))
          (set! y.1 (+ y.1 x.1))
          (begin (set! z.2 20) (set! z.3 (* z.3 z.4)) (begin (set! z.3 2) (halt t.1))))))
    `(module ((locals (,ls ...)))
        (begin
          (set! x.1 0)
          (set! y.1 x.1)
          (begin (set! a.3 2) (begin (set! b.2 (* b.2 h.9))) (set! a.3 9))
          (set! y.1 (+ y.1 x.1))
          (begin (set! z.2 20) (set! z.3 (* z.3 z.4)) (begin (set! z.3 2) (halt t.1)))))
    (equal? (list->set '(x.1 y.1 a.3 b.2 h.9 z.2 z.3 z.4 t.1)) (list->set ls)))

  ;assign-fvars tests
  (check-equal?
    (assign-fvars '(module ((locals ())) (halt 0)))
    '(module ((locals ()) (assignment ())) (halt 0)))
  (check-equal?
    (assign-fvars '(module ((other 2) (locals (tmp.1)) (other2 "hi")) (halt 0)))
    '(module ((other 2) (locals (tmp.1)) (other2 "hi") (assignment ((tmp.1 fv0)))) (halt 0)))
  (check-equal?
    (assign-fvars
      '(module
        ((locals (x.1 x.2 x.3)))
        (begin
          (begin (set! x.1 2) (set! y.1 (+ y.1 2)))
          (begin (set! x.2 19) (set! x.3 x.2) (halt 0)))))
    '(module
      ((locals (x.1 x.2 x.3)) (assignment ((x.1 fv0) (x.2 fv1) (x.3 fv2))))
      (begin
        (begin (set! x.1 2) (set! y.1 (+ y.1 2)))
        (begin (set! x.2 19) (set! x.3 x.2) (halt 0)))))
  
  ; replace-locations test
  (check-equal?
    (replace-locations
      '(module ((locals ()) (assignment ())) (halt 0)))
    '(halt 0))
  (check-equal?
    (replace-locations
      '(module ((locals (tmp.1)) (assignment ((tmp.1 rax)))) (halt tmp.1)))
    '(halt rax))
  (check-equal?
    (replace-locations
      '(module
        ((locals (x.1 x.2 x.3)) (assignment ((x.1 fv0) (z.2 fv1) (y.3 fv2) (a.7 rax) (b.9 r9))))
        (begin
          (set! x.1 2)
          (set! a.7 b.9)
          (set! z.2 (+ z.2 y.3))
          (begin
            (begin (set! a.7 y.3))
            (set! x.1 (* x.1 a.7))
            (begin (set! b.9 (+ b.9 2)) (set! a.7 (* a.7 x.1))))
          (begin (set! y.3 (+ y.3 20)) (halt y.3)))))
    '(begin
      (set! fv0 2)
      (set! rax r9)
      (set! fv1 (+ fv1 fv2))
      (begin
        (begin (set! rax fv2))
        (set! fv0 (* fv0 rax))
        (begin (set! r9 (+ r9 2)) (set! rax (* rax fv0))))
      (begin (set! fv2 (+ fv2 20)) (halt fv2))))

  ; assign-homes test
  (check-equal?
    (assign-homes '(module () (halt 0)))
    '(halt 0))
  (check-match
    (assign-homes '(module () (halt x.1)))
    `(halt ,fv0)
    (fvar? fv0))
  (check-match
    (assign-homes
      '(module
        ((other (1 2 3)) (other2 15) (other3 (other4 "hi")))
        (begin
          (set! x.1 2)
          (set! a.3 b.9)
          (begin
            (begin (set! a.7 y.3))
            (set! x.1 (* x.1 a.3))
            (begin (set! b.9 (+ b.9 2)) (set! a.7 (* a.7 x.1))))
          (set! z.2 (+ z.2 y.3))
          (begin (set! y.3 (+ y.3 20)) (halt 100)))))
    `(begin
      (set! ,fv0 2)
      (set! ,fv1 ,fv2)
      (begin
        (begin (set! ,fv3 ,fv4))
        (set! ,fv0 (* ,fv0 ,fv1))
        (begin (set! ,fv2 (+ ,fv2 2)) (set! ,fv3 (* ,fv3 ,fv0))))
      (set! ,fv5 (+ ,fv5 ,fv4))
      (begin (set! ,fv4 (+ ,fv4 20)) (halt 100)))
    (and
      (map fvar? (list fv0 fv1 fv2 fv3 fv4 fv5))
      (equal? (length (set->list (list->set (list fv0 fv1 fv2 fv3 fv4 fv5)))) 6)))

  ; check-values-lang and interp-values-lang tests
  (current-pass-list
   (list
    check-values-lang
    uniquify
    sequentialize-let
    normalize-bind
    select-instructions
    assign-homes
    flatten-begins
    patch-instructions
    implement-fvars
    check-paren-x64
    generate-x64
    wrap-x64-run-time
    wrap-x64-boilerplate))
  (define (check-and-interp p v)
    (check-equal? (check-values-lang p) p)
    (define interp-result (interp-values-lang p))
    (check-equal? interp-result v)
    (check-equal? interp-result (execute p)))
  (check-and-interp '(module 0) 0)
  (check-and-interp `(module ,(max-int 64)) (max-int 64))
  (check-and-interp '(module (* 15 6)) 90)
  (check-and-interp '(module (let () (+ 200 99))) 299)
  ; Shadowing name has same correct value
  (check-and-interp '(module (let ([x 2] [y (let ([a 15]) a)]) (let ([y (+ x y)]) (* y 2)))) 34)
  ; Shadow name from upper let and not the same let
  (check-and-interp '(module (let ([x 3]) (let ([x 5] [y x]) y))) 3)

  ; check-values-lang fail tests
  (check-exn exn:fail?
    (lambda ()
      (check-values-lang '(module (let ([a 2]) "hi")))))
  ; Duplicate name in let
  (check-exn exn:fail?
    (lambda ()
      (check-values-lang '(module (let ([a 2]) (let ([x 3] [x 4]) x))))))
  ; Using name from same let
  (check-exn exn:fail?
    (lambda ()
      (check-values-lang '(module (let ([x 3] [y x]) y)))))
  ; Invalid name in let
  (check-exn exn:fail?
    (lambda ()
      (check-values-lang '(module (let ([(let ([x 3]) 4) 3]) x)))))
  ; Undefined name test
  (check-exn exn:fail?
    (lambda ()
      (check-values-lang '(module (let ([x 3]) y)))))
  
  ; select-instructions tests
  (check-equal?
    (select-instructions '(module 1))
    '(module () (halt 1)))
  (check-equal?
    (select-instructions '(module x.1))
    '(module () (halt x.1)))
  (check-match
    (select-instructions '(module (+ 2 2)))
    `(module () (begin (set! ,tmp.1 2) (set! ,tmp.1 (+ ,tmp.1 2)) (halt ,tmp.1)))
    (aloc? tmp.1))
  (check-match
    (select-instructions
      '(module
        (begin
          (set! x.1 2)
          (begin
            (set! y.2 (+ y.2 5))
            (* 22 5)))))
    `(module ()
      (begin
        (set! x.1 2)
        (begin
          (set! y.2 (+ y.2 5))
          (begin
            (set! ,tmp.1 22)
            (set! ,tmp.1 (* ,tmp.1 5))
            (halt ,tmp.1)))))
    (and (aloc? tmp.1) (string-prefix? (symbol->string tmp.1) "tmp")))
  (check-equal?
    (select-instructions
      '(module
        (begin
          (set! x.1 2)
          (begin
            (set! z.3 4)
            (begin (set! b.5 (+ z.3 4)))
            (set! j.4 (+ 19 4)))
          (begin
            (set! y.2 5)
            y.2))))
    '(module ()
      (begin
        (set! x.1 2)
        (begin
          (set! z.3 4)
          (begin
            (set! b.5 z.3)
            (set! b.5 (+ b.5 4)))
          (set! j.4 19)
          (set! j.4 (+ j.4 4)))
        (begin
          (set! y.2 5)
          (halt y.2)))))
  
  ; normalize-bind tests
  (check-equal? (normalize-bind '(module 0)) '(module 0))
  (check-equal? (normalize-bind '(module (begin (+ 2 4)))) '(module (begin (+ 2 4))))
  (check-equal?
    (normalize-bind
      '(module
        (begin
          (set! x.1 (begin (set! x.1 5) x.1))
          x.1)))
    '(module
      (begin
        (begin
          (set! x.1 5)
          (set! x.1 x.1))
        x.1)))
  (check-equal?
    (normalize-bind
      '(module
        (begin
          (set! x.1 90)
          (set! x.1 (begin
                      (set! x.2 (begin (set! z.3 15) (set! y.1 (+ z.3 10)) (+ y.1 z.3)))
                      (set! a.2 (+ 4 x.2))
                      (set! a.2 (begin (set! a.2 (* a.2 10)) a.2))
                      a.2))
          (set! a.2 10)
          (begin
            (set! i.5 (begin (set! i.5 2) (* i.5 5)))
            i.5))))
    '(module
      (begin
        (set! x.1 90)
        (begin
          (begin (set! z.3 15) (set! y.1 (+ z.3 10)) (set! x.2 (+ y.1 z.3)))
          (set! a.2 (+ 4 x.2))
          (begin (set! a.2 (* a.2 10)) (set! a.2 a.2))
          (set! x.1 a.2))
        (set! a.2 10)
        (begin (begin (set! i.5 2) (set! i.5 (* i.5 5))) i.5))))
  
  ; sequentialize-let tests
  (check-equal? (sequentialize-let '(module 1)) '(module 1))
  (check-equal?
    (sequentialize-let
      '(module (let () (* 9 3))))
    '(module (begin (* 9 3))))
  (check-equal?
    (sequentialize-let
      '(module (let ([x 3] [y 2]) (+ x y))))
    '(module (begin (set! x 3) (set! y 2) (+ x y))))
  (check-equal?
    (sequentialize-let
      '(module
        (let ([x 3]
              [y (let ([a 3] [b 6] [y 2]) (let ([z (+ b y)]) (* a z)))])
             (+ x y))))
    '(module (begin
              (set! x 3)
              (set! y (begin
                        (set! a 3)
                        (set! b 6)
                        (set! y 2)
                        (begin (set! z (+ b y)) (* a z))))
              (+ x y))))
  
  ; uniquify tests
  (check-equal? (uniquify '(module 3)) '(module 3))
  (check-equal? (uniquify '(module (let () (+ 20 8)))) '(module (let () (+ 20 8))))
  (check-match
    (uniquify
      '(module
        (let ([x 3]
              [y (let ([x 3] [y 7] [z 2]) (let ([z (+ x y)]) (* y z)))])
             (let ([x (+ x 10)] [z 20]) (+ x y)))))
    `(module
        (let ([,x.1 3]
              [,y.2 (let ([,x.3 3] [,y.4 7] [,z.5 2]) (let ([,z.6 (+ ,x.3 ,y.4)]) (* ,y.4 ,z.6)))])
             (let ([,x.7 (+ ,x.1 10)] [,z.8 20]) (+ ,x.7 ,y.2))))))
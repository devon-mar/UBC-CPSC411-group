#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7)

(provide patch-instructions)

;; Milestone 2 Exercise 10
;; Milestone 4 Exercise 5
;; Milestone 6 Exercise 18
;; Milestone 7 Exercise 8
;;
;; Compile the Para-asm-lang v7 to Paren-x64 v7 by patching instructions that
;; have no x64 analogue into to a sequence of instructions and an auxiliary
;; register from current-patch-instructions-registers.
(define/contract (patch-instructions p)
  (-> para-asm-lang-v7? paren-x64-v7?)

  (define (big-int? i)
    (-> any/c boolean?)
    (and (int64? i)
         (not (int32? i))))

  (define (addr? a)
    (-> any/c boolean?)
    (match a
      [`(,fbp - ,dispoffset)
        #:when (and (frame-base-pointer-register? fbp)
                    (dispoffset? dispoffset))
        #t]
      [_ #f]))

  (define (patch-instructions-p p)
    (match p
      [`(begin ,s ...)
        `(begin
           ,@(append-map patch-instructions-s s))]))

  ;; Syntax:
  ;; (use-tmp
  ;;   ([value (or/c boolean? (-> any/c boolean?)) ...])
  ;;   (-> value/register? pare-x64-v7-s))
  ;;
  ;; value must be able to go in the RHS of a set!
  ;; f will be called with N args where N is the number of values given to use-tmp.
  ;; each arg will either be the given value or a temporary register.
  (define-syntax-rule (use-tmp ([vs bs ...] ...) f)
    (use-tmp-impl (list vs ...) (list (list bs ...) ...) f))
  ;; see above; do not call directly!
  (define (use-tmp-impl vs bs f)
    ;; Return a concrete boolean value from bs and v.
    (define/contract (eval-bs bs v)
      (-> (listof (or/c boolean? (-> any/c boolean?))) any/c boolean?)
      (let f ([bs bs])
        (cond
          [(empty? bs) #f]
          [(or (and (boolean? (car bs)) (car bs))
               (and (procedure? (car bs)) ((car bs) v)))
           #t]
          [else (f (cdr bs))])))

    (for/foldr ([new-vs '()]
                [s '()]
                [regs (current-patch-instructions-registers)]
                #:result `(,@s ,@(apply f new-vs)))
               ([v vs]
                [b bs])
      (cond
        [(empty? regs) (error "no more patch registers")]
        [(eval-bs b v)
         (values
           (cons (car regs) new-vs)
           (cons `(set! ,(car regs) ,v) s)
           (cdr regs))]
        [else (values (cons v new-vs) s regs)])))

  (define (patch-instructions-s s)
    (match s
      [`(set! ,loc (,b ,loc ,o))
        ;; para-asm-lang-v7        | Need tmp for o?
        ;; ------------------------|----------
        ;; loc/reg  opand/int32    | N
        ;; loc/reg  opand/int64    | Y
        ;; loc/reg  opand/loc/reg  | N
        ;; loc/reg  opand/loc/addr | N
        ;; loc/addr opand/int32    | N
        ;; loc/addr opand/int64    | Y
        ;; loc/addr opand/loc/reg  | N
        ;; loc/addr opand/loc/addr | N
        ;; always need a tmp for loc if addr
        (use-tmp
          ([loc addr?]
           [o big-int?])
          (lambda (r o)
            `((set! ,r (,b ,r ,o))
              ,@(if (addr? loc) `((set! ,loc ,r)) '()))))]
      [`(set! ,loc ,triv)
        ;; para-asm-lang-v7             | Need tmp?
        ;; -----------------------------|----------
        ;; loc/reg  triv/opand/int64    | N
        ;; loc/reg  triv/opand/loc/reg  | N
        ;; loc/reg  triv/opand/loc/addr | N
        ;; loc/reg  triv/label)         | N
        ;; loc/addr triv/opand/int32    | N
        ;; loc/addr triv/opand/int64    | Y
        ;; loc/addr triv/opand/loc/reg  | N
        ;; loc/addr triv/opand/loc/addr | Y
        ;; loc/addr triv/label          | N
        (use-tmp
          ([triv
             (and (addr? loc)
                  (or (big-int? triv) (addr? triv)))])
          (lambda (v) `((set! ,loc ,v))))]
      [`(jump ,trg)
        ;; label, reg, or addr -> reg or label
        (use-tmp
          ([trg addr?])
          (lambda (v) `((jump ,v))))]
      [`(with-label ,l ,s)
        (define stmts (patch-instructions-s s))
        `((with-label ,l ,(car stmts))
          ,@(cdr stmts))]
      [`(compare ,l ,o)
        (use-tmp
          ;; l must be a reg
          ([l addr?]
           ;; @278
           [o addr? big-int?])
          (lambda (r o) `((compare ,r ,o))))]
      [`(jump-if ,r ,t)
        (if (label? t)
          (list s)
          (let ([tmp-label (fresh-label 'tmp)])
            `((jump-if ,(invert-relop r) ,tmp-label)
              ;; paren-x64-fvars-v4 jump only supports a label or reg
              ;; so send it through again
              ,@(patch-instructions-s `(jump ,t))
              ;; some noop
              (with-label ,tmp-label (set! rax rax)))))]))


  ;; not used
  #;
  (define (patch-instructions-triv t)
    (match t
      [(? label?)
       (void)]
      [opand
        (void)]))

  ;; not used
  #;
  (define (patch-instructions-opand o)
    (match o
      [(? int64?)
       (void)]
      [loc
        (void)]))

  ;; not used
  #;
  (define (patch-instructions-trg t)
    (match t
      [(? label?)
       (void)]
      [loc
       (void)]))

  ;; not used
  #;
  (define (patch-instructions-loc l)
    (match l
      [(? register?)
       (void)]
      [addr (void)]))

  ;; not used
  #;
  (define (patch-instructions-binop b)
    (match b
      ['* (void)]
      ['+ (void)]
      ['- (void)]
      ['bitwise-and (void)]
 	 		['bitwise-ior (void)]
 	 	  ['bitwise-xor (void)]
 	 	  ['arithmetic-shift-right (void)]))

  ;; relop -> relop
  (define (invert-relop r)
    (match r
      ['< '>=]
      ['<= '>]
      ['= '!=]
      ['>= '<]
      ['> '<=]
      ['!= '=]))

  (patch-instructions-p p))

(module+ test
  (require rackunit)

  (define big-int (add1 (max-int 32)))
  (define 64-bit-int (max-int 64))
  (define rax (current-return-value-register))

  (define-check (check-42 p)
    (check-equal?
      (interp-paren-x64-v7 (patch-instructions p))
      42))

  (define/contract (fbp o)
    (-> dispoffset? any/c)
    `(,(current-frame-base-pointer-register) - ,o))

  ;; M6 tests

  (check-42
    ;; (set! loc/reg triv/opand/int32)
    `(begin (set! ,rax 42)))

  (check-42
    `(begin
       ;; (set! loc/reg triv/opand/int64)
       (set! ,rax ,(* 2 big-int))
       ;; (set! loc/reg (binop loc/reg opand/int64))
       (set! ,rax (- ,rax ,(- (* 2 big-int) 42)))))

  (check-42
    `(begin
       (set! rcx 42)
       ;; (set! loc/reg triv/opand/loc/reg)
       (set! ,rax rcx)))

  (check-42
    `(begin
       ;; (set! loc/addr triv/opand/int32)
       (set! ,(fbp 0) 42)
       ;; (set! loc/reg triv/opand/loc/addr)
       (set! ,rax ,(fbp 0))))

  (check-42
    `(begin
       ;; (set! loc/addr triv/opand/int64)
       (set! ,(fbp 0) ,(* 2 big-int))
       ;; (set! loc/addr (binop loc/addr opand/int64))
       (set! ,(fbp 0) (- ,(fbp 0) ,(- (* 2 big-int) 42)))
       (set! rax ,(fbp 0))))

  (check-42
    `(begin
       (set! rcx 7)
       ;; (set! loc/reg (binop loc/reg opand/int32))
       (set! rcx (* rcx 3))
       ;; (set! loc/addr triv/opand/loc/reg)
       (set! ,(fbp 0) rcx)
       ;; (set! loc/addr triv/opand/loc/addr)
       (set! ,(fbp 8) ,(fbp 0))
       ;; (set! loc/addr (binop loc/addr opand/int32))
       (set! ,(fbp 8) (* ,(fbp 8) 2))
       (set! ,rax ,(fbp 8))))

  (check-42
    `(begin
       (set! r8 1)
       (set! r9 2)
       ;; (set! loc/reg (binop loc/reg opand/loc/reg))
       (set! r9 (+ r9 r8))
       (set! ,(fbp 0) 3)
       ;; (set! loc/reg (binop loc/reg opand/loc/addr))
       (set! r9 (+ r9 ,(fbp 0)))
       (set! ,(fbp 0) r9)
       (set! rcx 4)
       ;; (set! loc/addr (binop loc/addr opand/loc/reg))
       (set! ,(fbp 0) (+ ,(fbp 0) rcx))
       (set! ,(fbp 8) 5)
       ;; (set! loc/addr (binop loc/addr opand/loc/addr))
       (set! ,(fbp 0) (* ,(fbp 0) ,(fbp 8)))
       (set! ,(fbp 8) 8)
       (set! ,(fbp 0) (- ,(fbp 0) ,(fbp 8)))
       (set! ,rax ,(fbp 0))))

  (check-42
    `(begin
       (set! ,rax 0)
       ;; (set! loc/reg triv/label)
       (set! rcx L.a.1)
       ;; (set! loc/addr triv/label)
       (set! ,(fbp 0) L.b.1)
       ;; (jump trg/loc/reg)
       (jump rcx)
       (set! ,rax (+ ,rax 1))
       ;; (with-label label s)
       ;; (jump trg/loc/addr)
       (with-label L.a.1 (jump ,(fbp 0)))
       (set! ,rax (+ ,rax 1))
       ;; (jump trg/label)
       (with-label L.b.1 (jump L.c.1))
       (set! ,rax (+ ,rax 1))
       (with-label L.c.1 (set! rsi 10))
       (compare rsi 10)
       ;; (jump-if relop trg/label)
       (jump-if = L.done.1)
       (set! ,rax (+ ,rax 1))
       (with-label L.done.1 (set! ,rax (+ ,rax 42)))))


  (check-42
    `(begin
       (set! r9 ,big-int)
       (set! rsi L.a.1)
       (set! rdi L.done.1)
       (set! ,rax 0)
       ;; (compare loc/reg opand/int64)
       (compare r9 ,big-int)
       ;; relop=/true
       (jump-if = rsi)
       (set! ,rax (+ ,rax 1))
       (with-label L.a.1 (compare r9, 10))
       ;; relop=/false
       (jump-if = rdi)
       (set! ,rax (+ ,rax 10))
       (with-label L.done.1 (set! ,rax (+ ,rax 32)))))

  (check-42
    `(begin
       (set! ,rax 0)
       (set! rsi L.a.1)
       (set! rdi L.done.1)
       (set! ,(fbp 0) 10)
       (set! rcx 10)
       (set! r9 8)
       ;; (compare loc/reg opand/loc/reg)
       (compare r9 rcx)
       ;; relop</true
       (jump-if < rsi)
       (set! ,rax (+ ,rax 1))
       ;; (compare loc/reg opand/loc/addr)
       (with-label L.a.1 (compare rcx ,(fbp 0)))
       ;; relop</false
       (jump-if < rdi)
       (set! ,rax (+ ,rax 10))
       (with-label L.done.1 (set! ,rax (+ ,rax 32)))))

  (check-42
    `(begin
       (set! ,rax 0)
       (set! rsi L.a.1)
       (set! rdi L.done.1)
       (set! r9 ,(sub1 big-int))
       ;; (compare loc/addr opand/int64)
       (compare r9 ,big-int)
       ;; relop<=/true
       (jump-if <= rsi)
       (set! ,rax (+ ,rax 1))
       (with-label L.a.1 (compare r9 2))
       ;; relop<=/false
       (jump-if <= rdi)
       (set! ,rax (+ ,rax 10))
       (with-label L.done.1 (set! ,rax (+ ,rax 32)))))

  (check-42
    `(begin
       (set! ,rax 0)
       (set! rsi L.a.1)
       (set! rdi L.done.1)
       (set! ,(fbp 0) 10)
       (set! ,(fbp 8) 20)
       (set! r9 5)
       ;; (compare loc/addr opand/loc/reg)
       (compare ,(fbp 0) r9)
       ;; relop>=/true
       (jump-if >= rsi)
       (set! ,rax (+ ,rax 1))
       ;; (compare loc/addr opand/loc/addr)
       (with-label L.a.1 (compare ,(fbp 0) ,(fbp 8)))
       ;; relop>=/false
       (jump-if >= rdi)
       (set! ,rax (+ ,rax 10))
       (with-label L.done.1 (set! ,rax (+ ,rax 32)))))

  (check-42
    `(begin
       (set! ,rax 0)
       (set! rsi L.a.1)
       (set! r9 10)
       (set! ,(fbp 0) L.done.1)
       (compare r9 4)
       ;; relop>/true
       (jump-if > rsi)
       (set! ,rax (+ ,rax 1))
       (with-label L.a.1 (compare r9 r9))
       ;; relop>/false
       ;; (jump-if relop trg/loc/addr)
       (jump-if > ,(fbp 0))
       (set! ,rax (+ ,rax 10))
       (with-label L.done.1 (set! ,rax (+ ,rax 32)))))

  (check-42
    `(begin
       (set! rsi L.a.1)
       (set! rdi L.done.1)
       (set! ,rax 0)
       (set! r9 10)
       (compare r9 4)
       ;; relop!=/true
       (jump-if != rsi)
       (set! ,rax (+ ,rax 1))
       (with-label L.a.1 (compare r9 r9))
       ;; relop!=/false
       ;; (jump-if relop trg/loc/reg)
       (jump-if != rdi)
       (set! ,rax (+ ,rax 10))
       (with-label L.done.1 (set! ,rax (+ ,rax 32)))))

  ;; Verify bitwise operations with 64-bit integers
  (check-42
    `(begin
       (set! rsi L.a.1)
       (set! rdi L.done.1)
       (set! ,rax 0)
       (set! r9 10)
       (set! r9 (bitwise-xor r9 r9))          ;; set to 0
       (set! r9 (bitwise-ior r9 2))           ;; set to 2
       (set! r9 (arithmetic-shift-right r9 2));; multiply by 4
       (set! r9 (bitwise-ior r9 2))           ;; add 2 since lower 2 bits are 0
       (set! r9 (bitwise-and r9 ,64-bit-int)) ;; bitwise-and with 64 bit all-ones integer
       (compare r9 4)
       ;; relop!=/true
       (jump-if != rsi)
       (set! ,rax (+ ,rax 1))
       (with-label L.a.1 (compare r9 r9))
       ;; relop!=/false
       ;; (jump-if relop trg/loc/reg)
       (jump-if != rdi)
       (set! ,rax (+ ,rax 10))
       (with-label L.done.1 (set! ,rax (+ ,rax 32)))))

  (parameterize ([current-patch-instructions-registers '()])
    (check-exn
      exn:fail?
      (lambda ()
        (patch-instructions
          `(begin
             (set! ,(fbp 0) ,big-int)
             (set! ,rax ,(fbp 0)))))))
  )

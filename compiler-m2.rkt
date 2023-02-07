#lang racket

(require
 cpsc411/compiler-lib
 cpsc411/2c-run-time)

(provide
 check-values-lang
 uniquify
 sequentialize-let
 normalize-bind
 select-instructions
 uncover-locals
 assign-fvars
 replace-locations
 assign-homes
 flatten-begins
 patch-instructions
 implement-fvars
 check-paren-x64
 generate-x64

 interp-values-lang

 interp-paren-x64)

;; String, Any -> error
;; Raise error with formatted message
(define (error-format e . o)
  (error (apply format e o)))

;; Any -> boolean
;; Return true if it is an addr, otherwise false
(define (addr? a)
  (match a
    [`(,fbp - ,dispoffset)
      #:when(and (frame-base-pointer-register? fbp)
                (dispoffset? dispoffset))
      #t]
    [_ #f]))

;; Paren-x64-v2 -> Paren-x64-v2 or error
;; Check if registers of Paren x64 program are initialized before use
;; and return the program or if not, raise an error with description
(define (check-paren-x64-init p)
  ; Paren-x64-v2 -> Paren-x64-v2 or error
  ; Check program to see if registers are initialized, raise error if not
  (define (check-program p)
    (match p
      [`(begin ,s ...)
       (begin
        (define regfile
          (for/fold ([regfile `(,(current-frame-base-pointer-register))])
                  ([i s])
                  (check-instruction regfile i)))
        (unless (member (current-return-value-register) regfile)
                (error "Register rax is not initialized in the program"))
        p)]))

  ; List, Paren-x64-instruction -> List or error
  ; Take the current initialized registers and update the list with the instruction
  (define (check-instruction regfile i)
    (match i
      [`(set! ,reg ,other)
       (begin (check-value regfile other) (append regfile (list reg)))]
      [`(set! (,fbp - ,dispoffset) ,other)
       (begin (check-value regfile other)
              (append regfile (list `(,fbp - ,dispoffset))))]))

  ; List, addr/reg/binop/int64/int32 -> void or error
  ; Check the Paren x64 value symbol to see if registers are initialized
  ; before use, return true or if not, raise error
  (define (check-value regfile v)
    (match v
      [`,reg #:when(register? reg)
        (unless (member reg regfile)
                (error-format "Register ~a is used before initialization" reg))]
      [`(,fbp - ,dispoffset)
        (unless (member `(,fbp - ,dispoffset) regfile)
                (error-format "Address (~a - ~a) is used before initialization" fbp dispoffset))]
      [`(+ ,reg ,other) (begin (check-value regfile reg) (check-value regfile other))]
      [`(* ,reg ,other) (begin (check-value regfile reg) (check-value regfile other))]
      [_ (void)]))

  (check-program p))

;; Any -> Paren-x64-v2 or error
;; Check if the syntax of Paren x64 program is correct
;; and return the program or if not, raise an error with description
(define (check-paren-x64-syntax p)  
  ; Any -> Paren-x64-v2 or error
  ; Check if the syntax of Paren x64 program is correct
  ; and return the program or if not, raise an error with description
  (define (check-program-syntax p)
    (match p
      [`(begin ,s ...) (begin (for-each check-statement-syntax s) p)]
      [_ (error "Invalid program")]))

  ; Any -> void or error
  ; Check if the syntax of Paren x64 statement is correct
  ; and return true or if not, raise error
  (define (check-statement-syntax s)
    (define (error-statement e . o)
      (define sep (if (empty? o) "" ";\n"))
      (error
        (apply format (string-append "Invalid statement ~a" sep e) s o)))
    (match s
      [`(set! (,fbp - ,dispoffset) ,other)
       (begin
        (unless
          (and (frame-base-pointer-register? fbp)
               (dispoffset? dispoffset))
          (error-statement "Expected (fbp - dispoffset), got (~a - ~a)" fbp dispoffset))
        (unless
          (or (int32? other) (register? other))
          (error-statement "Expected int32 or register, got ~a" other)))]
      [`(set! ,reg ,`(,binop ...)) #:when(or (equal? '+ (first binop)) (equal? '* (first binop)))
       (begin (unless
                (register? reg)
                (error-statement "Expected register, got ~a" reg))
              (check-binop-syntax reg binop))]
      [`(set! ,reg ,other)
       (begin (unless
                (register? reg)
                (error-statement "Expected register, got ~a" reg))
              (unless
                (or (int64? other) (register? other) (addr? other))
                (error-statement "Expected int64/register/addr, got ~a" other)))]
      [_ (error-statement "")]))

  ; Register, Any -> void or error
  ; Check if the syntax of Paren x64 binop is correct
  ; and return true or if not, raise error
  (define (check-binop-syntax r b)
    (define (error-binop e . o)
      (define sep (if (empty? o) "" ";\n"))
      (error
        (apply format (string-append "Invalid binop ~a" sep e) b o)))
    (match b
      [`(+ ,reg ,other)
       (begin (unless
                (equal? r reg)
                (error-binop "Expected register ~a, got ~a" r reg))
              (unless
                (or (int32? other) (register? other) (addr? other))
                (error-binop "Expected int32/register/addr, got ~a" other)))]
      [`(* ,reg ,other)
       (begin (unless
                (equal? r reg)
                (error-binop "Expected register ~a, got ~a" r reg))
              (unless
                (or (int32? other) (register? other) (addr? other))
                (error-binop "Expected int32/register/addr, got ~a" other)))]
      [_ (error-binop "")]))

  (check-program-syntax p))

;; M2 Exercise 15
;; Any -> Paren-x64-v2 or error
;; Check if input is valid Paren-x64 program,
;; return if it is, otherwise error
(define (check-paren-x64 p)
  (check-paren-x64-init (check-paren-x64-syntax p)))

;; M2 Exercise 16
;; Paren-x64-v2 -> Integer
;; Evaluate the Paren x64 v2 program to produce an exit code
(define (interp-paren-x64 p)
  ; Paren-x64-v2 -> Integer
  ; Evaluate the Paren x64 v2 program to produce an exit code
  (define (eval-program p)
    (match p
      [`(begin ,s ...)
       (define env
        (for/fold
          ([env (hash (current-frame-base-pointer-register) 0)])
          ([i s])
          (eval-instruction env i)))
       (modulo
        (dict-ref env (current-return-value-register)) 256)]))

  ; Environment, (Paren-x64-v2 instruction) -> Environment
  ; Evaluate the Paren x64 instruction and return the updated environment
  (define (eval-instruction env i)
    (match i
      [`(set! ,loc ,other) (dict-set env loc (eval-value env other))]))

  ; Environment, reg/addr/binop/int64/int32 -> Environment
  ; Evaluate the Paren x64 value-representing symbol to produce a value
  (define (eval-value env v)
    (match v
      [`,int #:when(int64? int) int]
      [`,loc #:when(or (register? loc) (addr? loc)) (dict-ref env loc)]
      [`(+ ,reg ,other) (x64-add (eval-value env reg) (eval-value env other))]
      [`(* ,reg ,other) (x64-mul (eval-value env reg) (eval-value env other))]))

  (eval-program p))

;; M2 Exercise 12
;; Paren-x64-v2 -> x64-instruction-sequence
;; Generate a x64 instruction sequence from a Paren x64 program
(define (generate-x64 p)
  ; Paren-x64-v2 -> x64-instruction-sequence
  (define (program->x64 p)
    (match p
      [`(begin ,s ...) (string-join (map statement->x64 s) "\n" #:after-last "\n")]))

  ; Paren-x64-v2-statement -> x64-instruction
  (define (statement->x64 s)
    (match s
      [`(set! (,fbp - ,dispoffset) ,other) (format "mov QWORD [~a - ~a], ~a" fbp dispoffset other)]
      [`(set! ,reg (,fbp - ,dispoffset)) (format "mov ~a, QWORD [~a - ~a]" reg fbp dispoffset)]
      [`(set! ,reg (,bin ...)) (binop->ins bin)]
      [`(set! ,reg ,other) (format "mov ~a, ~a" reg other)]))

  ; Paren-x64-v2-binop -> x64-instruction
  (define (binop->ins b)
    (match b
      [`(+ ,reg (,fbp - ,dispoffset)) (format "add ~a, QWORD [~a - ~a]" reg fbp dispoffset)]
      [`(* ,reg (,fbp - ,dispoffset)) (format "imul ~a, QWORD [~a - ~a]" reg fbp dispoffset)]
      [`(+ ,reg ,other) (format "add ~a, ~a" reg other)]
      [`(* ,reg ,other) (format "imul ~a, ~a" reg other)]))

  (program->x64 p))

;; M2 Exercise 11
;; Paren-x64-fvars-v2 -> Paren-x64-v2
;; Compile Paren-x64-fvars into Paren-x64
;; by converting fvars into displacement mode operands (addr)
(define (implement-fvars p)
  ; paren-x64-fvars-v2 fvar -> Paren-x64-v2 addr
  ; Convert a fvar into an addr
  (define (fvar->addr fvar)
    `(,(current-frame-base-pointer-register) - ,(* (fvar->index fvar) 8)))

  ; Paren-x64-fvars-v2 -> Paren-x64-v2
  (define (program->paren-x64 p)
    (match p
      [`(begin ,s ...) `(begin ,@(map statement->paren-x64 s))]))
  
  ; Paren-x64-fvars-v2 statement -> Paren-x64-v2 statement
  (define (statement->paren-x64 s)
    (match s
      [`(set! ,fvar ,other) #:when(fvar? fvar)
       `(set! ,(fvar->addr fvar) ,other)]
      [`(set! ,other ,fvar) #:when(fvar? fvar)
       `(set! ,other ,(fvar->addr fvar))]
      [`(set! ,other (,binop ,other ,fvar)) #:when(fvar? fvar)
       `(set! ,other (,binop ,other ,(fvar->addr fvar)))]
      [other other]))
  
  (program->paren-x64 p))

;; M2 Exercise 10
;; Para-asm-lang-v2 -> Paren-x64-fvars-v2
;; Compile Para-asm-lang into Paren-x64-fvars
;; by patching and replacing instructions that are not valid
;; such as fvar and int64 placement 
(define (patch-instructions p)
  ; Para-asm-lang-v2 -> Paren-x64-fvars-v2
  (define (program->x64-fvars p)
    (match p
      [`(begin ,s ... (halt ,triv))
       `(begin
        ,@(append
          (apply append (map (lambda (x) (statement->x64-fvars x (current-auxiliary-registers))) s))
          `((set! ,(current-return-value-register) ,triv))))]))

  ; Para-asm-lang-v2 statement -> (List-of (Paren-x64-fvars-v2 statement))
  (define (statement->x64-fvars s auxregs)
    (define aux (first auxregs))
    (match s
      [`(set! ,fvar (,binop ,fvar ,other)) #:when(fvar? fvar)
       `((set! ,aux ,fvar)
         ,@(statement->x64-fvars `(set! ,aux (,binop ,aux ,other)) (rest auxregs))
         (set! ,fvar ,aux))]
      [`(set! ,fvar ,other) #:when(and (fvar? fvar) (nor (int32? other) (register? other)))
       `((set! ,aux ,other)
         (set! ,fvar ,aux))]
      [`(set! ,reg (,binop ,reg ,other))
       #:when(and (register? reg) (nor (int32? other) (register? other) (fvar? other)))
       `((set! ,aux ,other)
         (set! ,reg (,binop ,reg ,aux)))]
      [_ `(,s)]))

  (program->x64-fvars p))

;; M2 Exercise 9
;; Nested-asm-lang-v2 -> Para-asm-lang-v2
;; Compile Nested-asm-lang into Para-asm-lang
;; by flattening all nested begins
(define (flatten-begins p)
  ; Nested-asm-lang-v2 -> Para-asm-lang-v2
  (define (flatten-program p)
    (match p
      [`(halt ,triv) `((halt ,triv))]
      [`(begin ,effects ... ,tail)
       `(,@(apply append (map flatten-effect effects)) ,@(flatten-program tail))]))
  
  ; Nested-asm-lang-v2 effect -> (List-of (Para-asm-lang-v2 effect))
  (define (flatten-effect e)
    (match e
      [`(begin ,effects ...) (apply append (map flatten-effect effects))]
      [_ `(,e)]))

  `(begin ,@(flatten-program p)))

;; M2 Exercise 6
;; Asm-lang-v2 -> Asm-lang-v2/locals
;; Compile Asm-lang to Asm-lang/locals
;; by adding information about all utilized
;; abstract locations into the info field
(define (uncover-locals p)
  ; Asm-lang-v2 -> Asm-lang-v2/locals
  (define (uncover-locals-module p)
    (match p
      [`(module ,info ,tail)
       `(module ,(info-set info 'locals (set->list (list->set (examine-tail tail)))) ,tail)]))
  
  ; Asm-lang-v2 -> (List-of (Asm-lang-v2/locals aloc))
  (define (examine-tail t)
    (match t
      [`(halt ,triv) (examine-triv triv)]
      [`(begin ,effects ... ,tail)
        (append (apply append (map examine-effect effects)) (examine-tail tail))]))
  
  ; Asm-lang-v2 -> (List-of (Asm-lang-v2/locals aloc))
  (define (examine-effect e)
    (match e
      [`(begin ,effects ...) (apply append (map examine-effect effects))]
      [`(set! ,aloc (,binop ,aloc ,triv)) `(,aloc ,@(examine-triv triv))]
      [`(set! ,aloc ,triv) `(,aloc ,@(examine-triv triv))]))
  
  ; Asm-lang-v2 -> (List-of (Asm-lang-v2/locals aloc))
  (define (examine-triv t)
    (match t
      [(? aloc?) `(,t)]
      [_ '()]))

  (uncover-locals-module p))

;; M2 Exercise 7
;; Asm-lang-v2/locals -> Asm-lang-v2/assignments
;; Compile Asm-lang/locals to Asm-lang/assignments
;; by assigning all utilized abstract locations
;; a new frame variable in the info field
(define (assign-fvars p)
  ; Asm-lang-v2/locals -> Asm-lang-v2/assignments
  (define (assign-module p)
    (match p
      [`(module ,info ,tail)
       `(module ,(info-set info 'assignment (generate-assign (info-ref info 'locals))) ,tail)]))
  
  ; (List-of (Asm-lang-v2/locals aloc))
  ; -> (List-of (Asm-lang-v2/assignments aloc and Asm-lang-v2/assignments fvar))
  ; Generate list of aloc to fvar assignments from list of alocs
  (define (generate-assign ls)
    (map
      (lambda (local index) `(,local ,(make-fvar index)))
      ls
      (range (length ls))))

  (assign-module p))

;; M2 Exercise 8
;; Asm-lang-v2/assignments -> Nested-asm-lang-v2
;; Compiles Asm-lang/assignments to Nested-asm-lang
;; by replacing each aloc with an fvar from the info field
(define (replace-locations p)
  (define asn (make-hash))

  ; Asm-lang-v2/assignments -> Nested-asm-lang-v2
  (define (replace-program p)
    (match p
      [`(module ,info ,tail)
       (for ([l (info-ref info 'assignment)])
        (dict-set! asn (first l) (second l)))
       (replace-tail tail)]))
  
  ; Asm-lang-v2/assignments tail -> Nested-asm-lang-v2 tail
  (define (replace-tail t)
    (match t
      [`(halt ,triv) `(halt ,(replace-triv triv))]
      [`(begin ,effects ... ,tail)
       `(begin ,@(map replace-effect effects) ,(replace-tail tail))]))
  
  ; Asm-lang-v2/assignments effect -> Nested-asm-lang-v2 effect
  (define (replace-effect e)
    (match e
      [`(begin ,effects ...)
       `(begin ,@(map replace-effect effects))]
      [`(set! ,aloc (,binop ,aloc ,triv))
       `(set! ,(dict-ref asn aloc) (,binop ,(dict-ref asn aloc) ,(replace-triv triv)))]
      [`(set! ,aloc ,triv)
       `(set! ,(dict-ref asn aloc) ,(replace-triv triv))]))
  
  ; Asm-lang-v2/assignments triv -> Nested-asm-lang-v2 triv
  (define (replace-triv t)
    (match t
      [(? aloc?) (dict-ref asn t)]
      [_ t]))

  (replace-program p))

;; M2 Exercise 5
;; Asm-lang-v2 -> nested-asm-lang-v2
;; Compile Asm-lang to Nested-asm-lang
;; by replacing each abstract location with a physical location
(define (assign-homes p)
  (replace-locations (assign-fvars (uncover-locals p))))

;; M2 Exercise 18
;; Values-lang-v3 -> int64
;; Interpret Values-lang program to return a value
(define (interp-values-lang p)
  ; Values-lang-v3 -> int64
  (define (interp-program p)
    (match p
      [`(module ,tail) (interp-value tail #hash())]))

  ; Values-lang-v3 value or tail -> int64
  (define (interp-value v env)
    (match v
      [`(let ([,xs ,vs] ...) ,value)
        (interp-value
          value
          (for/fold ([newenv env]) ([x xs] [v vs])
            (dict-set newenv x (interp-value v env))))]
      [`(+ ,triv1 ,triv2) (x64-add (interp-triv triv1 env) (interp-triv triv2 env))]
      [`(* ,triv1 ,triv2) (x64-mul (interp-triv triv1 env) (interp-triv triv2 env))]
      [_ (interp-triv v env)]))
  
  ; Values-lang-v3 triv -> int64
  (define (interp-triv t env)
    (match t
      [(? name?) (dict-ref env t)]
      [_ t]))

  (interp-program p))

;; M2 Exercise 17
;; Any -> Values-lang-v3 or error
;; Check if the input is a valid Values-lang program and returns it,
;; or raises an error 
(define (check-values-lang p)
  ; Any -> Values-lang-v3 or error
  (define (check-program p)
    (match p
      [`(module ,tail) (check-value tail '()) p]
      [_ (error "Invalid program")]))

  ; Any -> Void or error
  (define (check-value v env)
    (match v
      [`(let ([,xs ,vs] ...) ,value)
        (when (check-duplicates xs) (error "Invalid let; duplicate names"))
        (check-value
          value
          (for/fold ([newenv env]) ([x xs] [v vs])
            (unless (name? x) (error-format "Invalid let; ~a is not name" x))
            (check-value v env)
            (set-add newenv x)))]
      [`(+ ,triv1 ,triv2) (check-triv triv1 env) (check-triv triv2 env)]
      [`(* ,triv1 ,triv2) (check-triv triv1 env) (check-triv triv2 env)]
      [_ (check-triv v env)]))
  
  ; Any -> Void or error
  (define (check-triv t env)
    (match t
      [(? name?)
       (unless (set-member? env t) (error-format "Invalid triv; name ~a is not defined" t))]
      [(? int64?) (void)]
      [_ (error-format "Invalid triv; ~a is not name or int64" t)]))

  (check-program p))

;; M2 Exercise 4
;; Imp-cmf-lang-v3 -> Asm-lang-v2
;; Compile Imp-cmf-lang v3 to Asm-lang v2,
;; for instructions with no direct corresponding instruction,
;; select proper set of instructions with alocs to replace 
(define (select-instructions p)
  ; (Imp-cmf-lang-v3 value) -> (List-of (Asm-lang-v2 effect)) and (Asm-lang-v2 aloc)
  ; Assigns the value v to a fresh temporary, returning two values: the list of
  ; statements the implement the assignment in Loc-lang, and the aloc that the
  ; value is stored in.
  (define (assign-tmp v)
    (define x (fresh))
    `(((set! ,x ,v)) ,x))

  ; Imp-cmf-lang-v3 tail -> Asm-lang-v2 tail
  (define (select-tail t)
    (match t
      [`(begin ,effects ... ,tail)
       `(begin ,@(apply append (map select-effect effects)) ,(select-tail tail))]
      [`(,binop ,triv1 ,triv2) 
       (let ([asn (assign-tmp triv1)])
        `(begin
          ,@(apply append (map select-effect (first asn)))
          (set! ,(second asn) (,binop ,(second asn) ,triv2))
          (halt ,(second asn))))]
      [value `(halt ,value)]))
  
  ; Imp-cmf-lang-v3 effect -> (List-of (Asm-lang-v2 effect))
  ; Replace effect with multiple effects if it is an invalid effect in Asm-lang-v2
  (define (select-effect e)
    (match e
      [`(set! ,aloc (,binop ,triv1 ,triv2)) #:when(not (equal? aloc triv1))
       `((set! ,aloc ,triv1) (set! ,aloc (,binop ,aloc ,triv2)))]
      [`(set! ,aloc ,value) `(,e)]
      [`(begin ,effects ...)
       `((begin ,@(apply append (map select-effect effects))))]))

  (match p
    [`(module ,tail)
     `(module () ,(select-tail tail))]))

;; M2 Exercise 3
;; imp-mf-lang-v3 -> imp-cmf-lang-v3
;; Compile Imp-mf-lang to Imp-cmf-lang by moving the set and begin
(define (normalize-bind p)
  ; imp-mf-lang-v3 tail -> imp-cmf-lang-v3 tail
  (define (normalize-tail t)
    (match t
      [`(begin ,effects ... ,tail)
       `(begin ,@(map normalize-effect effects) ,(normalize-tail tail))]
      [_ t]))
  
  ; imp-mf-lang-v3 effect -> imp-cmf-lang-v3 effect
  (define (normalize-effect e)
    (match e
      [`(set! ,aloc (begin ,effects ... ,value))
       `(begin ,@(map normalize-effect effects) (set! ,aloc ,value))]
      [`(begin ,effects ...)
       `(begin ,@(map normalize-effect effects))]
      [_ e]))

  (match p
    [`(module ,tail)
     `(module ,(normalize-tail tail))]))

;; M2 Exercise 2
;; Values-unique-lang-v3 -> Imp-mf-lang-v3
;; Compile Values-unique-lang to Imp-mf-lang by converting "let" to "begin"
(define (sequentialize-let p)
  ; Values-unique-lang-v3 tail -> Imp-mf-lang-v3 tail
  (define (sequentialize-tail t)
    (match t
      [`(let ([,alocs ,values] ...) ,tail)
       `(begin
        ,@(for/list ([a alocs] [v values])
          `(set! ,a ,(sequentialize-value v)))
        ,(sequentialize-tail tail))]
      [value (sequentialize-value value)]))

  ; Values-unique-lang-v3 value -> Imp-mf-lang-v3 value
  (define (sequentialize-value v)
    (match v
      [`(let ([,alocs ,values] ...) ,value)
       `(begin
        ,@(for/list ([a alocs] [v values])
          `(set! ,a ,(sequentialize-value v)))
        ,(sequentialize-value value))]
      [triv triv]))

  (match p
    [`(module ,tail)
     `(module ,(sequentialize-tail tail))]))

;; M2 Exercise 1
;; Values-lang-v3 -> Values-unique-lang-v3
;; Compile Values-lang to Values-unique-lang by resolving all names to alocs
;; where alocs are globally unique
(define (uniquify p)
  ; (Dictionary (Values-lang-v3 name, Values-unique-lang-v3 aloc)) and Values-unique-lang-v3 names
  ; -> (Dictionary (Values-lang-v3 name, Values-unique-lang-v3 aloc))
  ; Get the environment with all names assigned to fresh alocs
  (define (get-new-env env names)
    (for/fold ([newenv env]) ([name names])
      (dict-set newenv name (fresh name))))

  ; Values-lang-v3 tail -> Values-unique-lang-v3 tail
  (define (uniquify-tail t env)
    (match t
      [`(let ([,xs ,values] ...) ,tail)
        (define newenv (get-new-env env xs))
       `(let
          ,(for/list ([x xs] [v values])
            `[,(dict-ref newenv x) ,(uniquify-value v env)])
          ,(uniquify-tail tail newenv))]
      [value (uniquify-value value env)]))
  
  ; Values-lang-v3 value -> Values-unique-lang-v3 value
  (define (uniquify-value v env)
    (match v
      [`(let ([,xs ,values] ...) ,value)
        (define newenv (get-new-env env xs))
       `(let
          ,(for/list ([x xs] [v values])
            `[,(dict-ref newenv x) ,(uniquify-value v env)])
          ,(uniquify-value value newenv))]
      [`(,binop ,triv1 ,triv2) `(,binop ,(uniquify-triv triv1 env) ,(uniquify-triv triv2 env))]
      [triv (uniquify-triv triv env)]))
  
  ; Values-lang-v3 triv -> Values-unique-lang-v3 triv
  (define (uniquify-triv t env)
    (match t
      [(? name?) (dict-ref env t)]
      [_ t]))

  (match p
    [`(module ,tail)
     `(module ,(uniquify-tail tail #hash()))]))

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
  generate-x64
  wrap-x64-run-time
  wrap-x64-boilerplate))

(module+ test
  (require
   rackunit
   rackunit/text-ui
   cpsc411/test-suite/public/v3
   ;; NB: Workaround typo in shipped version of cpsc411-lib
   (except-in cpsc411/langs/v3 values-lang-v3)
   cpsc411/langs/v2)

  (run-tests
   (v3-public-test-sutie
    (current-pass-list)
    (list
     interp-values-lang-v3
     interp-values-lang-v3
     interp-values-unique-lang-v3
     interp-imp-mf-lang-v3
     interp-imp-cmf-lang-v3
     interp-asm-lang-v2
     interp-nested-asm-lang-v2
     interp-para-asm-lang-v2
     interp-paren-x64-fvars-v2
     interp-paren-x64-v2
     #f #f))))

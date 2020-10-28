;Assignment 5

#lang typed/racket
(require typed/rackunit)

(define EPSILON 1e-05)

;-------------------------------------------------------------------------------------------
; Data Definitions:

;Creating the data-types of both the ExprC
(define-type ExprC (U NumC IdC AppC LamC IfC StringC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct LamC ([params : (Listof Symbol)] [body : ExprC])  #:transparent)
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct StringC ([str : String]) #:transparent)

;define Environment and Store
(define-type Location Natural)
(define-type Env (Listof Binding))
(define-type Store (Mutable-HashTable Location Value))

(struct Binding ((id : Symbol) (val : Location)) #:transparent)
(struct Cell ((location : Location) (val : Value)))

;define Value
(define-type Value (U NumV PrimV CloV BoolV StringV NullV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StringV ([str : String]) #:transparent)
(struct PrimV ([fun : (-> (Listof Value) Value)]) #:transparent)
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [clo-env : Env]) #:transparent)
(struct NullV ()#:transparent)

;-------------------------------------------------------------------------------------------

;;Evaluates a given an S-Expression as a DXUQ4 expression, returns a value as a string
(define (top-interp [sexps : Sexp]) : String
 (serialize (interp (parse sexps) top-env top-store)))

;-----------------------------------------------------------------------------------------

;;Evaluates an ExprC given an Environment, returns a Value
(define (interp [a : ExprC] [env : Env] [store : Store]) : Value
   (match a
     
     [(NumC n) (NumV n)]

     ;[(StringC str) (StringV str)]
     
     [(IdC s) (fetch (env-lookup s env) store)]

     #;[(IfC test then else) (match (interp test env)
                             [(BoolV #t) (interp then env)]
                             [(BoolV #f) (interp else env)]
                             [else (error 'interp "DXUQ if condition is not a boolean")])]

     ;[(LamC param body) (CloV param body env)]
     
     [(AppC fun args)
      
      ;evaluate expression that should return a function value
      (define funval (interp fun env store))
      
      (match funval
        
        [(PrimV fun) (fun (map (λ ([x : ExprC]) (interp x env store)) args))]

        #;[(CloV params body clo-env)

         ;;verify correct number of arguments
         (cond
           [(not (equal? (length params) (length args)))
            (error 'interp "DXUQ incorrect number of arguments passed to function")])

         ;evaluate arguments (eager)
         (define arg_vals (map (λ ([x : ExprC]) (interp x env store)) args))

         ;extend CloV environment with parameters
         (define new-env (env-extend-all params arg_vals clo-env))
         
         ;evaluate the function body
         (interp body new-env store)]
        
        [else (error 'interp (string-append "DXUQ invalid function call: " (~v funval)))])]))

;;----------------------------------------------------------------------------------------------

;;Serializes a DXUQ value to its string representation
(define (serialize [v : Value]) : String
  (match v
    [(NumV n) (~v n)]
    [(StringV str) str]
    [(CloV params body env) "#<procedure>"]
    [(PrimV fun) "#<primop>"]
    [(BoolV b) (cond
                    [(equal? b #t) "true"]
                    [else "false"])]))

;-------------------------------------------------------------------------------------------
;define Arithmetic functions

;;primitive addition - adds two NumV's together
(define (add [args : (Listof Value)]) : Value
  (match args
    [(list (? NumV? a) (? NumV? b)) (NumV (+ (NumV-n a) (NumV-n b)))]
    [else (error '+ (string-append "DXUQ invalid arguments passed to +: " (~v args)))]))

;;primitive subtraction - subtracts two NumV's
(define (subtract [args : (Listof Value)]) : Value
  (match args
    [(list (? NumV? a) (? NumV? b)) (NumV (- (NumV-n a) (NumV-n b)))]
    [else (error '+ (string-append "DXUQ invalid arguments passed to -: " (~v args)))]))

;;primitive multiplication - multiply two NumV's
(define (multiply [args : (Listof Value)]) : Value
  (match args
    [(list (? NumV? a) (? NumV? b)) (NumV (* (NumV-n a) (NumV-n b)))]
    [else (error '+ (string-append "DXUQ invalid arguments passed to *: " (~v args)))]))

;;primitive division - divide two NumV's
(define (divide [args : (Listof Value)]) : Value
  (match args
    [(list (NumV left) (NumV right))
     (cond
       [(equal? right 0) (error 'divide
                                (string-append "DXUQ division by zero: " (~v left) " / 0"))]
       [(NumV (/ left right))])]
    [else (error '+ (string-append "DXUQ invalid arguments passed to /: " (~v args)))]))

;;primitive less than or equal to - compare two NumV's with <=
(define (leq [args : (Listof Value)]) : Value
  (match args
    [(list (NumV left) (NumV right)) (BoolV (<= left right))]
    [else (error '+ (string-append "DXUQ invalid arguments passed to <=: " (~v args)))]))

;;primitive equality - compare two NumV's with equal?
(define (eq [args : (Listof Value)]) : Value
  (match args
    [(list left right) (BoolV (equal? left right))]
    [else (error '+ (string-append "DXUQ invalid arguments passed to equal?: " (~v args)))]))

;;signals a user error
(define (user-error [args : (Listof Value)]) : Value
  (match args
    [(list arg) (error 'user-error (string-append "DXUQ " (serialize arg)))]
    [else (error (string-append "DXUQ invalid arguments passed to error: " (~v args)))]))

;-------------------------------------------------------------------------------------------

;;grabs the location of an id in an environment
(define (env-lookup [id : Symbol] [env : Env]) : Location
  (cond
    [(empty? env)
     (error 'env-lookup (string-append "DXUQ unbound identifier: " (~a id)))]
    [(equal? id (Binding-id (first env))) (Binding-val (first env))]
    [else (env-lookup id (rest env))]))

;-------------------------------------------------------------------------------------------

;;grabs the value of a location in a store 
(define (fetch [loc : Location] [store : Store]) : Value
  (cond
    [(hash-has-key? store loc) (hash-ref store loc)]
    [else (NullV)]))

;-------------------------------------------------------------------------------------------

;;Given a list of identifiers and a list of values, adds each identifier-value pair to an env
(define (env-store-extend-all [ids : (Listof Symbol)] [args : (Listof Value)] [env : Env] [store : Store]) : Env
  (cond
    [(empty? ids) env]
    [else
     (store-extend )
     (env-store-extend-all
           (rest ids)
           (rest args)
           (env-extend (first ids) (first args) env) store)]))

;-------------------------------------------------------------------------------------------

;;Adds a single binding to an environment
(define (env-extend [param : Symbol] [loc : Location] [env : Env]) : Env
  (cons (Binding param loc) env))

;-------------------------------------------------------------------------------------------

;;Adds a single cell to a store
(define (store-extend [loc : Location] [val : Value] [store : Store]) : Void
  (hash-set! store loc val))

;-------------------------------------------------------------------------------------------

;;Gets the next available location from a store

;-----------------------------------------------------------------------------------------

;;Given an SExpression, parse returns an ExprC struct representing the SExpression
(define (parse [s : Sexp]) : ExprC
 (match s
   [(? real? a) (NumC a)]
   [(? string? str) (StringC str)]
   [(? symbol? id)
    (cond
      [(valid-idc id) (IdC id)]
      [else (error 'parse (string-append "DXUQ invalid identifier: " (~a id)))])]
   [(list 'fn (list (? symbol? params) ...) body)
    (LamC (validate-params (cast params (Listof Symbol))) (parse body))]  
   [(list 'let defs ... 'in body) (desugar-let (cast defs (Listof Sexp)) body)]
   [(list 'if exprs ...)
    (cond
      [(equal? (length exprs) 3)
       (IfC (parse (first exprs)) (parse (second exprs)) (parse (third exprs)))]
      [else (error 'parse (string-append "DXUQ invalid syntax for if: " (~a s)))])]
   [(list fun args ...) (AppC (parse fun) (map parse args))]
   [else (error 'parse (string-append "DXUQ invalid input to parse: " (~a s)))]))


;;Desugars a 'let' s-expression into an AppC
(define (desugar-let [defs : (Listof Sexp)] [body : Sexp]) : AppC
  (define var-names (map get-var-name defs))
  (define dup (check-duplicates var-names))
  (cond
    [(not (equal? dup #f))
     (error (string-append "DXUQ duplicate variable definition in let: " (~a dup)))])
  (AppC (LamC (map get-var-name defs) (parse body))
        (map (λ ([x : Sexp]) (parse (get-var-value x))) defs)))

;;parse a variable definition, return the variable name
(define (get-var-name [def : Sexp]) : Symbol
  (match def
    [(list (? symbol? name) '= value) name]
    [else (error (string-append "DXUQ invalid variable definition: " (~a def)))]))

;;parse a variable definition, return the variable value
(define (get-var-value [def : Sexp]) : Sexp
  (match def
    [(list (? symbol? name) '= value) value]
    [else (error (string-append "DXUQ invalid variable definition: " (~a def)))]))

;;verifies a function has no duplicate parameters, returns the list if no duplicates exist
(define (validate-params [params : (Listof Symbol)]) : (Listof Symbol)
  (define dup (check-duplicates params))
  (match dup
    [#f params]
    [else (error (string-append "DXUQ duplicate parameter: " (~a dup)))]))


;-----------------------------------------------------------------------------------------

;;Determines if a given identifier is a valid identifier
(define (valid-idc [id : Sexp]) : Boolean
  (match id
    ['let #f]
    ['in #f]
    ['if #f]
    ['fn #f]
    ['vars #f]
    ['lam #f]
    [else #t]))


;;----------------------------------------------------------------------------------------------

;define top-env, loaded with primitive values, passed in to interp
(define top-env (list (Binding '+ 0)
                      (Binding '* 1)
                      (Binding '/ 2)
                      (Binding '- 3)
                      (Binding '<= 4)
                      (Binding 'equal? 5)
                      (Binding 'error 6)
                      (Binding 'true 7)
                      (Binding 'false 8)))

;define top-store
(: top-store (Mutable-HashTable Location Value))
(define top-store (make-hash))
(hash-set! top-store 0 (PrimV add))
(hash-set! top-store 1 (PrimV multiply))
(hash-set! top-store 2 (PrimV divide))
(hash-set! top-store 3 (PrimV subtract))
(hash-set! top-store 4 (PrimV leq))
(hash-set! top-store 5 (PrimV eq))
(hash-set! top-store 6 (PrimV user-error))
(hash-set! top-store 7 (BoolV #t))
(hash-set! top-store 8 (BoolV #f))

;;----------------------------------------------------------------------------------------------
;Test Cases

;Test Cases for the top-interp 
(check-equal? (top-interp '{+ 4 5}) "9")
(check-equal? (top-interp '{{fn {x y} {* x y}} 3 7}) "21")
(check-equal? (top-interp '{{fn {x add4} {add4 x}} 20 {fn {x} {+ x 4}}}) "24")
(check-equal? (top-interp
               '{{fn {add1 dotwice} {dotwice add1 14}}
                 {fn {x} {+ x 1}} {fn {f a} {f {f a}}}}) "16")
(check-equal? (top-interp '{{fn {y} {{fn {fun y} {fun}} {fn {} y} 100}} 1}) "1")
(check-equal? (top-interp '{let {x = 10} {y = 2} in {* x y}}) "20")
(check-equal? (top-interp '{let {x = {/ 144 12}} {y = {* 2 3}} in {* x y}}) "72")
(check-equal? (top-interp 'true) "true")
(check-equal? (top-interp 'false) "false")
(check-equal? (top-interp '{if true 10 20}) "10")
(check-equal? (top-interp '{if false 10 20}) "20")
(check-equal? (top-interp '"testing") "testing")
(check-equal? (top-interp '{<= 10 20}) "true")
(check-equal? (top-interp '{equal? 10 20}) "false")
(check-equal? (top-interp '{equal? 20 20}) "true")
(check-exn (regexp (regexp-quote "DXUQ duplicate parameter: x"))
          (lambda () (top-interp '{{fn {x x} {+ x x}} 20 30})))
(check-exn (regexp (regexp-quote "DXUQ invalid syntax for if"))
          (lambda () (top-interp '{if {equal? 10 10} 5})))
(check-exn (regexp (regexp-quote "DXUQ incorrect number of arguments passed to function"))
          (lambda () (top-interp '{{fn {x} {+ x 1}} 20 20})))
(check-exn (regexp (regexp-quote "DXUQ invalid function call"))
          (lambda () (top-interp '{{+ 5 5} 20 20})))

;Test Cases for the interp
(check-equal? (interp (AppC (IdC '+) (list (NumC 4) (NumC 5))) top-env) (NumV 9))
(check-equal? (interp (AppC (IdC '-) (list (NumC 10) (NumC 5))) top-env) (NumV 5))
(check-equal? (interp (AppC (IdC '*) (list (NumC 10) (NumC 5))) top-env) (NumV 50))
(check-equal? (interp (IdC 'true) top-env) (BoolV #t))
(check-equal? (interp (IdC 'false) top-env) (BoolV #f))
(check-equal? (interp (StringC '"test string") top-env) (StringV "test string"))
(check-equal? (interp (IfC (IdC 'true) (NumC 10) (NumC 20)) top-env) (NumV 10))
(check-equal? (interp (IfC (IdC 'false) (NumC 10) (NumC 20)) top-env) (NumV 20))
(check-equal? (interp (AppC
                       (LamC (list 'x) (AppC (IdC '+) (list (IdC 'x) (NumC 1)))) (list (NumC 5)))
                      top-env) (NumV 6))
(check-equal? (interp(AppC
                      (LamC (list 'x 'y)
                            (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))) (list (NumC 25)(NumC 5)))
                      top-env) (NumV 30))
(check-exn (regexp (regexp-quote "DXUQ invalid function call"))
          (lambda () (interp (AppC (NumC 5) (list (NumC 4) (NumC 5))) top-env)))
(check-exn (regexp (regexp-quote "DXUQ if condition is not a boolean"))
          (lambda () (interp (IfC (NumC 10) (NumC 4) (NumC 8)) top-env)))
(check-exn (regexp (regexp-quote "DXUQ division by zero: 10 / 0"))
          (lambda () (interp (AppC (IdC '/) (list (NumC 10) (NumC 0))) top-env)))
(check-exn (regexp (regexp-quote "DXUQ division by zero: 20 / 0"))
          (lambda () (interp (AppC (IdC '/)
                                   (list (NumC 20)
                                         (AppC (IdC '-) (list (NumC 25) (NumC 25))))) top-env)))

;Test cases for env-lookup
(check-equal? (env-lookup 'var (list (Binding 'var (NumV 4)))) (NumV 4))
(check-equal? (env-lookup 'true top-env) (BoolV #t))
(check-equal? (env-lookup 'false top-env) (BoolV #f))
(check-exn (regexp (regexp-quote "DXUQ unbound identifier: x"))
          (lambda () (env-lookup 'x (list (Binding 'y (NumV 4))))))

;Test cases for env-extend
(check-equal? (env-extend 'var (NumV 4) (list (Binding 'x (NumV 10))))
              (list (Binding 'var (NumV 4)) (Binding 'x (NumV 10))))

;Test cases for env-extend-all
(check-equal? (env-extend-all '(x y) (list (NumV 4) (NumV 5)) '())
              (list (Binding 'y (NumV 5)) (Binding 'x (NumV 4))))

;Test cases for add
(check-equal? (add (list (NumV 4) (NumV 10))) (NumV 14))
(check-exn (regexp (regexp-quote "DXUQ invalid arguments passed to +"))
          (lambda () (add (list (NumV 4) (NumV 10) (NumV 12)))))

;Test cases for subtract
(check-equal? (subtract (list (NumV 25) (NumV 4))) (NumV 21))
(check-exn (regexp (regexp-quote "DXUQ invalid arguments passed to -"))
          (lambda () (subtract (list (NumV 4) (NumV 10) (NumV 12)))))

;Test cases for multiply
(check-equal? (multiply (list (NumV 5) (NumV 12))) (NumV 60))
(check-exn (regexp (regexp-quote "DXUQ invalid arguments passed to *"))
          (lambda () (multiply (list (NumV 4) (NumV 10) (NumV 12)))))

;Test cases for divide
(check-equal? (divide (list (NumV 36) (NumV 6))) (NumV 6))
(check-exn (regexp (regexp-quote "DXUQ invalid arguments passed to /"))
          (lambda () (divide (list (NumV 4) (NumV 10) (NumV 12)))))

;Test cases for leq
(check-equal? (leq (list (NumV 5) (NumV 12))) (BoolV #t))
(check-equal? (leq (list (NumV 14) (NumV 12))) (BoolV #f))
(check-exn (regexp (regexp-quote "DXUQ invalid arguments passed to <="))
          (lambda () (leq (list (NumV 4) (NumV 10) (NumV 12)))))
(check-exn (regexp (regexp-quote "DXUQ invalid arguments passed to <="))
          (lambda () (leq (list (NumV 4) (NumV 10) (BoolV #t)))))

;Test cases for eq
(check-equal? (eq (list (NumV 12) (NumV 12))) (BoolV #t))
(check-equal? (eq (list (StringV "test1") (StringV "test1"))) (BoolV #t))
(check-equal? (eq (list (BoolV #t) (BoolV #t))) (BoolV #t))
(check-equal? (eq (list (StringV "test1") (StringV "test2"))) (BoolV #f))
(check-exn (regexp (regexp-quote "DXUQ invalid arguments passed to equal?"))
          (lambda () (eq (list (NumV 4) (NumV 10) (NumV 12)))))
(check-exn (regexp (regexp-quote "DXUQ invalid arguments passed to equal?"))
          (lambda () (eq (list (NumV 10) (NumV 10) (NumV 10)))))

;Test cases for error
(check-exn (regexp (regexp-quote "user-error: DXUQ bad data"))
          (lambda () (user-error (list (StringV "bad data")))))
(check-exn (regexp (regexp-quote "DXUQ invalid arguments passed to error"))
          (lambda () (user-error (list (StringV "bad data") (StringV "test")))))

;Test Cases for the parse
(check-equal? (parse 'true) (IdC 'true))
(check-equal? (parse 'false) (IdC 'false))
(check-equal? (parse '"test string") (StringC "test string"))
(check-equal? (parse '{if true 1 2}) (IfC (IdC 'true) (NumC 1) (NumC 2)))
(check-equal? (parse '{+ 1 2}) (AppC (IdC '+) (list (NumC 1) (NumC 2))))
(check-equal? (parse '{fn {x y} {* x y}})
              (LamC (list 'x 'y) (AppC (IdC '*) (list (IdC 'x) (IdC 'y)))))
(check-equal? (parse '{{fn {x y} {* x y}} 2 16})
              (AppC (LamC (list 'x 'y) (AppC (IdC '*) (list (IdC 'x) (IdC 'y))))
                    (list (NumC 2) (NumC 16))))
(check-equal? (parse '{let {z = {+ 9 14}} {y = 98} in {+ z y}})
              (AppC (LamC '(z y) (AppC (IdC '+) (list (IdC 'z) (IdC 'y))))
                    (list (AppC (IdC '+) (list (NumC 9) (NumC 14))) (NumC 98))))
(check-exn (regexp (regexp-quote "DXUQ invalid identifier: in"))
          (lambda () (parse '{{fn {in y} {* in y}} 2 16})))
(check-exn (regexp (regexp-quote "DXUQ invalid input to parse: ()"))
          (lambda () (parse '{})))

;Test Cases for desugar-let
(check-equal? (desugar-let (list '(z = 4)) '(+ z 1))
              (AppC (LamC '(z) (AppC (IdC '+) (list (IdC 'z) (NumC 1))))
                    (list (NumC 4))))
(check-exn (regexp (regexp-quote "DXUQ duplicate variable definition in let: z"))
          (lambda () (desugar-let (list '(z = 4) '(z = 5)) '(+ z 1))))

;Test Cases for get-var-name
(check-equal? (get-var-name '(var = 45)) 'var)
(check-exn (regexp (regexp-quote "DXUQ invalid variable definition: (var == 45)"))
          (lambda () (get-var-name '(var == 45))))

;Test Cases for validate-params
(check-equal? (validate-params '(x y z b)) '(x y z b))
(check-exn (regexp (regexp-quote "DXUQ duplicate parameter: x"))
          (lambda () (validate-params '(x y z x))))

;Test Cases for get-var-value
(check-equal? (get-var-value '(var = 45)) '45)
(check-equal? (get-var-value '(var = {* 4 5})) '(* 4 5))
(check-exn (regexp (regexp-quote "DXUQ invalid variable definition: (var == 45)"))
          (lambda () (get-var-value '(var == 45))))

;Test Cases for the valid-idc
(check-equal? (valid-idc 'x) #t)
(check-equal? (valid-idc '+) #t)
(check-equal? (valid-idc 'if) #f)
(check-equal? (valid-idc 'fn) #f)
(check-equal? (valid-idc 'let) #f)
(check-equal? (valid-idc 'in) #f)
(check-equal? (valid-idc 'vars) #f)
(check-equal? (valid-idc 'lam) #f)

;Test Cases for serialize
(check-equal? (serialize (NumV 10)) "10")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (PrimV add)) "#<primop>")
(check-equal? (serialize
               (CloV '(x y)
                     (AppC (IdC '+) (list (NumC 4) (NumC 5))) top-env))
              "#<procedure>")

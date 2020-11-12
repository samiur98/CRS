;Assignment 6: DXUQ6 Type Checking and Recursion.

;The project has been finished. All functions have been implemented and they
;have been tested by suites of test cases that test all main functions as well
;as helper functions.

#lang typed/racket
(require typed/rackunit)

(define EPSILON 1e-05)

;-------------------------------------------------------------------------------------------
; Data Definitions:

;Creating the data-types of both the ExprC and the FunDefC
(define-type ExprC (U NumC IdC AppC LamC IfC StringC RecC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct LamC ([params : (Listof Symbol)] [types : (Listof Type)] [body : ExprC]) #:transparent)
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct StringC ([str : String]) #:transparent)
(struct RecC ([funName : Symbol] [def : LamC] [output : Type] [use : ExprC]) #:transparent)

;define Environment
(define-type Env (Listof Binding))
(struct Binding ((id : Symbol) (val : (Boxof Value))) #:transparent)

;define TypeEnvironment
(define-type TypeEnv (Listof TypeBinding))
(struct TypeBinding ((id : Symbol) (type : Type)) #:transparent)

;define Value
(define-type Value (U NumV PrimV CloV BoolV StringV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StringV ([str : String]) #:transparent)
(struct PrimV ([fun : (-> (Listof Value) Value)]) #:transparent)
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [clo-env : Env]) #:transparent)

;define Type
(define-type Type (U NumT BoolT StringT FunT))
(struct NumT () #:transparent)
(struct BoolT () #:transparent)
(struct StringT () #:transparent)
(struct FunT ([paramTypes : (Listof Type)] [retType : Type]) #:transparent)

;-------------------------------------------------------------------------------------------

;;Evaluates a given an S-Expression as a DXUQ6 expression, returns a value as a string
(define (top-interp [sexps : Sexp]) : String
  (define AST (parse sexps))
  (type-check AST base-tenv)
  (serialize (interp AST top-env)))

;-----------------------------------------------------------------------------------------

;;Evaluates an ExprC given an Environment, returns a Value
(define (interp [a : ExprC] [env : Env]) : Value
   (match a
     
     [(NumC n) (NumV n)]

     [(StringC str) (StringV str)]
     
     [(IdC s) (env-lookup s env)]

     [(IfC test then else) (match (interp test env)
                             [(BoolV #t) (interp then env)]
                             [(BoolV #f) (interp else env)]
                             [else (error 'interp "DXUQ if condition is not a boolean")])]

     [(LamC param types body) (CloV param body env)]
     
     [(AppC fun args)
      
      ;evaluate expression that should return a function value
      (define funval (interp fun env))
      
      (match funval
        
        [(PrimV fun) (fun (map (λ ([x : ExprC]) (interp x env)) args))]

        [(CloV params body clo-env)

         ;evaluate arguments (eager)
         (define arg_vals (map (λ ([x : ExprC]) (box (interp x env))) args))

         ;extend CloV environment with parameters
         (define new-env (env-extend-all params arg_vals clo-env))
         
         ;evaluate the function body
         (interp body new-env)]
        
        [else (error 'interp (string-append "DXUQ invalid function call: " (~v funval)))])]
     
     [(RecC name def retType use)
      
      ;add recursive function name to environment with a temporary value
      (define new-env (env-extend name (NumV 0) env))

      ;interp the recursive function body to get a CloV
      (define clo-val (interp def new-env))

      ;Mutate environment with correct function value
      (env-set! name clo-val new-env)

      ;interp the expression that uses the function, using the new env
      (interp use new-env)]))


;;----------------------------------------------------------------------------------------------

;;Verifies that an ExprC is correctly typed by returning its type or throwing an error
(define (type-check [expr : ExprC] [tenv : TypeEnv]) : Type
  (match expr
    [(NumC n) (NumT)]
    
    [(StringC str) (StringT)]
    
    [(IdC id) (tenv-lookup id tenv)]
    
    [(IfC test then else)
     (define then-type (type-check then tenv))
     (cond
       [(not (BoolT? (type-check test tenv)))
        (error 'type-check "DXUQ if condition not a boolean")]
       [(not (equal? (type-check else tenv) then-type))
        (error 'type-check "DXUQ if 'then' and 'else' must return the same type")])
     then-type]
    
    [(LamC params types body)
     (FunT types (type-check body (tenv-extend-all params types tenv)))]

    [(AppC fun args)
     (define fun-type (type-check fun tenv))
     (cond [(not (FunT? fun-type))
            (error 'type-check (string-append "DXUQ invalid function call: " (~v fun)))])
     (verify-all-arg-types (FunT-paramTypes fun-type) args tenv)
     (FunT-retType fun-type)]

    [(RecC name def retType use)
     (define new-tenv
       (tenv-extend name (FunT (LamC-types def) retType) tenv))
     (define body-type
       (type-check (LamC-body def) (tenv-extend-all (LamC-params def) (LamC-types def) new-tenv)))
     (cond [(not (equal? body-type retType))
            (error 'type-check "DXUQ recursive function return type does not match expected")])
     (type-check use new-tenv)]))

;;Verifies that a function application is given the correct parameter types, true if correct
(define (verify-all-arg-types [paramTypes : (Listof Type)]
                          [args : (Listof ExprC)]
                          [tenv : TypeEnv]) : Boolean
  (cond
    [(not (equal? (length paramTypes) (length args)))
     (error 'verify-arg-types "DXUQ incorrect number of arguments passed to function")]
    [(equal? (length paramTypes) 0)]
    [else
     (verify-arg-type (first paramTypes) (first args) tenv)
     (verify-all-arg-types (rest paramTypes) (rest args) tenv)]))

;;Verifies that a single parameter and argument are of the same type, returns true if they are
(define (verify-arg-type [paramType : Type] [arg : ExprC] [tenv : TypeEnv]) : Boolean
  (define arg-type (type-check arg tenv))
  (cond
    [(equal? paramType arg-type)]
    [else (error 'verify-arg-type
                 (string-append "DXUQ wrong argument type passed to function:"
                                " expected: " (~v paramType)
                                " -- actual: " (~v arg-type)))]))


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
;define primitive functions

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

;;prmitive multiplication - multiply two NumV's
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

;;primitive number equality - compare two NumV's with equal?
(define (num-eq? [args : (Listof Value)]) : Value
  (match args
    [(list (? NumV? left) (? NumV? right)) (BoolV (equal? left right))]
    [else (error '+ (string-append "DXUQ invalid arguments passed to num-eq?: " (~v args)))]))

;;primitive string equality - compare two StringV's with equal?
(define (str-eq? [args : (Listof Value)]) : Value
  (match args
    [(list (? StringV? left) (? StringV? right)) (BoolV (equal? left right))]
    [else (error '+ (string-append "DXUQ invalid arguments passed to str-eq?: " (~v args)))]))

;;signals a user error
(define (user-error [args : (Listof Value)]) : Value
  (match args
    [(list arg) (error 'user-error (string-append "DXUQ " (serialize arg)))]
    [else (error (string-append "DXUQ invalid arguments passed to error: " (~v args)))]))


;-------------------------------------------------------------------------------------------

;;grabs the value of an id in a given environment
(define (env-lookup [id : Symbol] [env : Env]) : Value
  (cond
    [(empty? env)
     (error 'env-lookup (string-append "DXUQ unbound identifier: " (~a id)))]
    [(equal? id (Binding-id (first env))) (unbox (Binding-val (first env)))]
    [else (env-lookup id (rest env))]))

;-------------------------------------------------------------------------------------------

;;sets the value of an id in a given environment
(define (env-set! [id : Symbol] [newVal : Value] [env : Env]) : Void
  (cond
    [(empty? env)
     (error 'env-set! (string-append "DXUQ unbound identifier: " (~a id)))]
    [(equal? id (Binding-id (first env))) (set-box! (Binding-val (first env)) newVal)]
    [else (env-set! id newVal (rest env))]))

;-------------------------------------------------------------------------------------------

;;Given a list of identifiers and a list of values, adds each identifier-value pair to an env
;;returns the new env
(define (env-extend-all [ids : (Listof Symbol)] [args : (Listof (Boxof Value))] [env : Env]) : Env
  (cond
    [(empty? ids) env]
    [else (env-extend-all
           (rest ids)
           (rest args)
           (env-extend (first ids) (unbox (first args)) env))]))

;-------------------------------------------------------------------------------------------

;;Adds a single binding to an environment, returns new env
(define (env-extend [param : Symbol] [arg : Value] [env : Env]) : Env
  (cons (Binding param (box arg)) env))

;-------------------------------------------------------------------------------------------

;;returns the type of an id in a given type environment
(define (tenv-lookup [id : Symbol] [tenv : TypeEnv]) : Type
  (cond
    [(empty? tenv)
     (error 'tenv-lookup (string-append "DXUQ unbound identifier: " (~a id)))]
    [(equal? id (TypeBinding-id (first tenv))) (TypeBinding-type (first tenv))]
    [else (tenv-lookup id (rest tenv))]))

;-------------------------------------------------------------------------------------------

;;Given a list of identifiers and a list of values, adds each identifier-value pair to a tenv
;;returns new tenv
(define (tenv-extend-all [ids : (Listof Symbol)]
                         [types : (Listof Type)]
                         [tenv : TypeEnv]) : TypeEnv
  (cond
    [(empty? ids) tenv]
    [else (tenv-extend-all
           (rest ids)
           (rest types)
           (tenv-extend (first ids) (first types) tenv))]))

;-------------------------------------------------------------------------------------------

;;Adds a single binding to an environment, returns new tenv
(define (tenv-extend [param : Symbol] [type : Type] [tenv : TypeEnv]) : TypeEnv
  (cons (TypeBinding param type) tenv))

;-----------------------------------------------------------------------------------------

;;Given an SExpression, returns an ExprC struct representing the SExpression
(define (parse [s : Sexp]) : ExprC
 (match s
   [(? real? a) (NumC a)]
   [(? string? str) (StringC str)]
   [(? symbol? id)
    (cond
      [(valid-idc id) (IdC id)]
      [else (error 'parse (string-append "DXUQ invalid identifier: " (~a id)))])]
   [(list 'fn (list (? list? params) ...) body)
    (LamC (validate-params(map get-param-name (cast params (Listof Sexp))))
          (map get-param-type (cast params (Listof Sexp)))
          (parse body))]  
   [(list 'let defs ... 'in body) (desugar-let (cast defs (Listof Sexp)) body)]
   [(list 'if exprs ...)
    (cond
      [(equal? (length exprs) 3)
       (IfC (parse (first exprs)) (parse (second exprs)) (parse (third exprs)))]
      [else (error 'parse (string-append "DXUQ invalid syntax for if: " (~a s)))])]
   [(list 'rec (list (list (? symbol? name) (? list? params) ...) ': retType body) use)
          (RecC name
                (LamC (validate-params (map get-param-name (cast params (Listof Sexp))))
                      (map get-param-type (cast params (Listof Sexp)))
                      (parse body))
                (parse-type retType)
                (parse use))]
   [(list fun args ...) (AppC (parse fun) (map parse args))]
   [else (error 'parse (string-append "DXUQ invalid input to parse: " (~a s)))]))
                                         

;;Parse an s-expression that holds a Type
(define (parse-type [s : Sexp]) : Type
  (match s
    ['num (NumT)]
    ['bool (BoolT)]
    ['str (StringT)]
    [(list p-types ... '-> ret-type)
     (FunT (map parse-type (cast p-types (Listof Sexp))) (parse-type ret-type))]
    [else (error 'parse-type (string-append "DXUQ invalid type:" (~a s)))]))


;;Desugars a 'let' s-expression into an AppC
(define (desugar-let [defs : (Listof Sexp)] [body : Sexp]) : AppC
  (define var-names (map get-var-name defs))
  (define dup (check-duplicates var-names))
  (cond
    [(not (equal? dup #f))
     (error (string-append "DXUQ duplicate variable definition in let: " (~a dup)))])
  (AppC (LamC var-names (map get-var-type defs) (parse body))
        (map (λ ([x : Sexp]) (parse (get-var-value x))) defs)))

;;parse a variable definition, return the variable name
(define (get-var-name [def : Sexp]) : Symbol
  (match def
    [(list type (? symbol? name) '= value) name]
    [else (error (string-append "DXUQ invalid variable definition: " (~a def)))]))

;;parse a variable definition, return the variable value
(define (get-var-value [def : Sexp]) : Sexp
  (match def
    [(list type (? symbol? name) '= value) value]
    [else (error (string-append "DXUQ invalid variable definition: " (~a def)))]))

;;parse a variable definition, return the variable type
(define (get-var-type [def : Sexp]) : Type
  (match def
    [(list type (? symbol? name) '= value) (parse-type type)]
    [else (error (string-append "DXUQ invalid variable definition: " (~a def)))]))

;;parse a parameter definition, return the parameter name
(define (get-param-name [def : Sexp]) : Symbol
  (match def
    [(list type (? symbol? name)) name]
    [else (error (string-append "DXUQ invalid parameter definition: " (~a def)))]))

;;parse a parameter definition, return the parameter type
(define (get-param-type [def : Sexp]) : Type
  (match def
    [(list type (? symbol? name)) (parse-type type)]
    [else (error (string-append "DXUQ invalid parameter definition: " (~a def)))]))

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

;define top-env, loaded with primitive values, passed to interp
(define top-env (list (Binding '+ (box (PrimV add)))
                      (Binding '* (box (PrimV multiply)))
                      (Binding '/ (box (PrimV divide)))
                      (Binding '- (box (PrimV subtract)))
                      (Binding '<= (box (PrimV leq)))
                      (Binding 'num-eq? (box (PrimV num-eq?)))
                      (Binding 'str-eq? (box (PrimV str-eq?)))
                      (Binding 'error (box (PrimV user-error)))
                      (Binding 'true (box (BoolV #t)))
                      (Binding 'false (box (BoolV #f)))))

;define base-tenv, loaded with types for primitive values, passed to type-check
(define base-tenv (list (TypeBinding '+ (FunT (list (NumT) (NumT)) (NumT)))
                       (TypeBinding '* (FunT (list (NumT) (NumT)) (NumT)))
                       (TypeBinding '/ (FunT (list (NumT) (NumT)) (NumT)))
                       (TypeBinding '- (FunT (list (NumT) (NumT)) (NumT)))
                       (TypeBinding '<= (FunT (list (NumT) (NumT)) (BoolT)))
                       (TypeBinding 'num-eq? (FunT (list (NumT) (NumT)) (BoolT)))
                       (TypeBinding 'str-eq? (FunT (list (StringT) (StringT)) (BoolT)))
                       (TypeBinding 'true (BoolT))
                       (TypeBinding 'false (BoolT))))


;;----------------------------------------------------------------------------------------------
;Test Cases

;Test Cases for the top-interp 
(check-equal? (top-interp '{+ 4 5}) "9")
(check-equal? (top-interp '{{fn {[num x] [num y]} {* x y}} 3 7}) "21")
(check-equal? (top-interp '{{fn {[num x] [{num -> num} add4]} {add4 x}}
                            20 {fn {[num x]} {+ x 4}}}) "24")
(check-equal? (top-interp
               '{{fn {[{num -> num} add1] [{{num -> num} num -> num} dotwice]}
                     {dotwice add1 14}}
                 {fn {[num x]} {+ x 1}}
                 {fn {[{num -> num} f] [num a]} {f {f a}}}}) "16")
(check-equal? (top-interp '{{fn {[num y]}
                                {{fn {[{-> num} fun] [num y]}
                                     {fun}} {fn {} y} 100}} 1}) "1")
(check-equal? (top-interp '{rec {{fact [num x]} : num
                                                {if {<= x 1}
                                                    1
                                                    {* x {fact {- x 1}}}}}
                             {fact 5}}) "120")
(check-equal? (top-interp '{let {num x = 10} {num y = 2} in {* x y}}) "20")
(check-equal? (top-interp '{let {num x = {/ 144 12}} {num y = {* 2 3}} in {* x y}}) "72")
(check-equal? (top-interp 'true) "true")
(check-equal? (top-interp 'false) "false")
(check-equal? (top-interp '{if true 10 20}) "10")
(check-equal? (top-interp '{if false 10 20}) "20")
(check-equal? (top-interp '"testing") "testing")
(check-equal? (top-interp '{<= 10 20}) "true")
(check-equal? (top-interp '{num-eq? 10 20}) "false")
(check-equal? (top-interp '{num-eq? 20 20}) "true")
(check-equal? (top-interp '{str-eq? "hello" "hello"}) "true")
(check-equal? (top-interp '{str-eq? "hi" "hii"}) "false")
(check-exn (regexp (regexp-quote "DXUQ duplicate parameter: x"))
          (lambda () (top-interp '{{fn {[num x] [num x]} {+ x x}} 20 30})))
(check-exn (regexp (regexp-quote "DXUQ invalid syntax for if"))
          (lambda () (top-interp '{if {num-eq? 10 10} 5})))
(check-exn (regexp (regexp-quote "DXUQ incorrect number of arguments passed to function"))
          (lambda () (top-interp '{{fn {[num x]} {+ x 1}} 20 20})))
(check-exn (regexp (regexp-quote "DXUQ invalid function call"))
          (lambda () (top-interp '{{+ 5 5} 20 20})))
(check-exn (regexp (regexp-quote "DXUQ wrong argument type passed to function"))
          (lambda () (top-interp '{+ 20 "hello"})))
(check-exn (regexp (regexp-quote "DXUQ wrong argument type passed to function"))
          (lambda () (top-interp '{{fn {[num x]} x} true})))
(check-exn (regexp (regexp-quote "DXUQ wrong argument type passed to function"))
          (lambda () (top-interp '{{fn {[{num -> str} x]} x} {fn {[num y]} y}})))

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
                       (LamC '(x) (list (NumT)) (AppC (IdC '+) (list (IdC 'x) (NumC 1))))
                       (list (NumC 5))) top-env) (NumV 6))
(check-equal? (interp(AppC
                      (LamC '(x y) (list (NumT) (NumT))
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

;Test cases for type-check
(check-equal? (type-check (NumC 5) '()) (NumT))
(check-equal? (type-check (StringC "str") '()) (StringT))
(check-equal? (type-check (AppC (IdC '+)
                                (list (NumC 5) (AppC (IdC '/) (list (NumC 100) (NumC 2)))))
                          base-tenv) (NumT))
(check-equal? (type-check (IdC 'x)
                          (list(TypeBinding 'y (NumT))(TypeBinding 'x (BoolT))))(BoolT))
(check-equal? (type-check (IfC (IdC 'true)(StringC "then")(StringC "else")) base-tenv)(StringT))
(check-equal? (type-check (LamC '(x) (list (NumT)) (AppC (IdC '+) (list (IdC 'x) (NumC 1))))
                          base-tenv)(FunT (list (NumT)) (NumT)))
(check-equal? (type-check (RecC 'recurs
                                (LamC '(x) (list (NumT)) (AppC (IdC 'recurs) (list (IdC 'x))))
                                (NumT)
                                (AppC (IdC 'recurs) (list (NumC 5))))
                          base-tenv) (NumT))
(check-exn (regexp (regexp-quote "DXUQ if condition not a boolean"))
          (lambda () (type-check (IfC (NumC 5) (StringC "then") (StringC "else"))'())))
(check-exn (regexp (regexp-quote "DXUQ if 'then' and 'else' must return the same type"))
          (lambda () (type-check (IfC (IdC 'true) (StringC "then") (NumC 10)) base-tenv)))
(check-exn (regexp (regexp-quote "DXUQ invalid function call"))
          (lambda () (type-check (AppC (NumC 5) '()) base-tenv)))
(check-exn (regexp (regexp-quote "DXUQ recursive function return type does not match expected"))
          (lambda () (type-check (RecC 'recurs
                                (LamC '(x) (list (NumT)) (StringC "abc"))
                                (NumT)
                                (AppC (IdC 'recurs) (list (NumC 5)))) base-tenv)))

;Test cases for verify-all-arg-types
(check-equal? (verify-all-arg-types (list (NumT) (StringT))
                                    (list (NumC 3) (StringC "str")) '()) #t)
(check-exn (regexp (regexp-quote "DXUQ incorrect number of arguments passed to function"))
          (lambda () (verify-all-arg-types (list (NumT))
                                    (list (NumC 3) (StringC "str")) '())))

;Test cases for verify-arg-type
(check-equal? (verify-arg-type (NumT) (NumC 5) '()) #t)
(check-exn (regexp (regexp-quote "DXUQ wrong argument type passed to function"))
          (lambda () (verify-arg-type (BoolT) (NumC 5) '())))

;Test cases for env-lookup
(check-equal? (env-lookup 'var (list (Binding 'var (box (NumV 4))))) (NumV 4))
(check-equal? (env-lookup 'true top-env) (BoolV #t))
(check-equal? (env-lookup 'false top-env) (BoolV #f))
(check-exn (regexp (regexp-quote "DXUQ unbound identifier: x"))
          (lambda () (env-lookup 'x (list (Binding 'y (box (NumV 4)))))))

;Test cases for env-set!
(define test-env (list (Binding 'var1 (box (NumV 4)))
                       (Binding 'var2 (box (BoolV #t)))))
(check-equal? (env-lookup 'var1 test-env) (NumV 4))
(env-set! 'var1 (StringV "test") test-env)
(check-equal? (env-lookup 'var1 test-env) (StringV "test"))
(check-exn (regexp (regexp-quote "DXUQ unbound identifier: x"))
          (lambda () (env-set! 'x (NumV 5) test-env)))

;Test cases for env-extend
(check-equal? (env-extend 'var (NumV 4) (list (Binding 'x (box (NumV 10)))))
              (list (Binding 'var (box (NumV 4))) (Binding 'x (box (NumV 10)))))

;Test cases for env-extend-all
(check-equal? (env-extend-all '(x y) (list (box (NumV 4)) (box (NumV 5))) '())
              (list (Binding 'y (box (NumV 5))) (Binding 'x (box (NumV 4)))))

;Test cases for tenv-lookup
(check-exn (regexp (regexp-quote "DXUQ unbound identifier: x"))
          (lambda () (tenv-lookup 'x (list (TypeBinding 'y (NumT))))))
(check-exn (regexp (regexp-quote "DXUQ unbound identifier: x"))
          (lambda () (tenv-lookup 'x (list (TypeBinding 'y (NumT))))))

;Test cases for env-extend
(check-equal? (tenv-extend 'var (NumT) (list (TypeBinding 'x (BoolT))))
              (list (TypeBinding 'var (NumT)) (TypeBinding 'x (BoolT))))

;Test cases for env-extend-all
(check-equal? (tenv-extend-all '(x y) (list (NumT) (BoolT)) '())
              (list (TypeBinding 'y (BoolT)) (TypeBinding 'x (NumT))))

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

;Test cases for num-eq?
(check-equal? (num-eq? (list (NumV 12) (NumV 12))) (BoolV #t))
(check-equal? (num-eq? (list (NumV 12) (NumV 11))) (BoolV #f))
(check-exn (regexp (regexp-quote "DXUQ invalid arguments passed to num-eq?"))
          (lambda () (num-eq? (list (NumV 4) (NumV 10) (NumV 12)))))
(check-exn (regexp (regexp-quote "DXUQ invalid arguments passed to num-eq?"))
          (lambda () (num-eq? (list (NumV 4) (BoolV #t)))))

;Test cases for str-eq?
(check-equal? (str-eq? (list (StringV "test1") (StringV "test1"))) (BoolV #t))
(check-equal? (str-eq? (list (StringV "test1") (StringV "test2"))) (BoolV #f))
(check-exn (regexp (regexp-quote "DXUQ invalid arguments passed to str-eq?"))
          (lambda () (str-eq? (list (StringV "test1") (StringV "test1") (StringV "test1")))))
(check-exn (regexp (regexp-quote "DXUQ invalid arguments passed to str-eq?"))
          (lambda () (str-eq? (list (StringV "test1") (NumV 4)))))

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
(check-equal? (parse '{fn {[num x] [num y]} {* x y}})
              (LamC '(x y) (list (NumT)(NumT)) (AppC (IdC '*) (list (IdC 'x) (IdC 'y)))))
(check-equal? (parse '{{fn {[num x] [num y]} {* x y}} 2 16})
              (AppC (LamC '(x y) (list (NumT)(NumT)) (AppC (IdC '*) (list (IdC 'x) (IdC 'y))))
                    (list (NumC 2) (NumC 16))))
(check-equal? (parse '{let {num z = {+ 9 14}} {num y = 98} in {+ z y}})
              (AppC (LamC '(z y) (list (NumT)(NumT)) (AppC (IdC '+) (list (IdC 'z) (IdC 'y))))
                    (list (AppC (IdC '+) (list (NumC 9) (NumC 14))) (NumC 98))))
(check-equal? (parse '{rec {{addition [num x] [num y]} : num {addition y x}} {addition 5 10}})
              (RecC 'addition (LamC (list 'x 'y) (list (NumT) (NumT))
                                    (AppC (IdC 'addition) (list (IdC 'y) (IdC 'x))))
                                          (NumT)
                                    (AppC (IdC 'addition) (list (NumC 5) (NumC 10)))))
(check-exn (regexp (regexp-quote "DXUQ invalid identifier: in"))
          (lambda () (parse '{{fn {[num in] [num y]} {* in y}} 2 16})))
(check-exn (regexp (regexp-quote "DXUQ invalid input to parse: ()"))
          (lambda () (parse '{})))
(check-exn (regexp (regexp-quote "DXUQ invalid syntax for if"))
          (lambda () (parse '{if {<= 5 0} 10})))

;Test Cases for parse-type
(check-equal? (parse-type 'num) (NumT))
(check-equal? (parse-type 'bool) (BoolT))
(check-equal? (parse-type 'str) (StringT))
(check-equal? (parse-type '{num num -> num}) (FunT (list (NumT) (NumT)) (NumT)))
(check-equal? (parse-type '{bool str num -> {num -> str}})
              (FunT (list (BoolT)(StringT)(NumT))
                    (FunT (list (NumT)) (StringT))))
(check-equal? (parse-type '{{str -> bool} -> num})
              (FunT (list (FunT (list (StringT)) (BoolT))) (NumT)))
(check-equal? (parse-type '{-> num})
              (FunT '() (NumT)))
(check-exn (regexp (regexp-quote "DXUQ invalid type"))
          (lambda () (parse-type '{num -> 14})))

;Test Cases for desugar-let
(check-equal? (desugar-let (list '(num z = 4)) '(+ z 1))
              (AppC (LamC '(z) (list (NumT)) (AppC (IdC '+) (list (IdC 'z) (NumC 1))))
                    (list (NumC 4))))
(check-exn (regexp (regexp-quote "DXUQ duplicate variable definition in let: z"))
          (lambda () (desugar-let (list '(num z = 4) '(num z = 5)) '(+ z 1))))

;Test Cases for get-var-name
(check-equal? (get-var-name '(num var = 45)) 'var)
(check-exn (regexp (regexp-quote "DXUQ invalid variable definition: (num var == 45)"))
          (lambda () (get-var-name '(num var == 45))))

;Test Cases for get-var-value
(check-equal? (get-var-value '(num var = 45)) '45)
(check-equal? (get-var-value '(num var = {* 4 5})) '(* 4 5))
(check-exn (regexp (regexp-quote "DXUQ invalid variable definition: (num var == 45)"))
          (lambda () (get-var-value '(num var == 45))))

;Test Cases for get-var-type
(check-equal? (get-var-type '(num var = 45)) (NumT))
(check-equal? (get-var-type '(str var = "hi")) (StringT))
(check-exn (regexp (regexp-quote "DXUQ invalid variable definition: (num var == 45)"))
          (lambda () (get-var-type '(num var == 45))))

;Test Cases for get-param-name
(check-equal? (get-param-name '[num var]) 'var)
(check-exn (regexp (regexp-quote "DXUQ invalid parameter definition: (num var x)"))
          (lambda () (get-param-name '[num var x])))

;Test Cases for get-param-type
(check-equal? (get-param-type '[bool var]) (BoolT))
(check-exn (regexp (regexp-quote "DXUQ invalid parameter definition: (num var x)"))
          (lambda () (get-param-type '[num var x])))

;Test Cases for validate-params
(check-equal? (validate-params '(x y z b)) '(x y z b))
(check-exn (regexp (regexp-quote "DXUQ duplicate parameter: x"))
          (lambda () (validate-params '(x y z x))))

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
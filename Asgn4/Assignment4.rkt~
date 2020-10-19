;Assignment 4: DXUQ4 Parser and Interpreter with multiple arguments.

;The project has been finished. All functions have been implemented and they
;have been tested by a suite of test cases that test all main functions as well
;as helper functions.

#lang typed/racket

(require typed/rackunit)

(define EPSILON 1e-05)

;-------------------------------------------------------------------------------------------
; Data Definitions:

;Creating the data-types of both the ExprC and the FunDefC
(define-type ExprC (U NumC IdC AppC LamC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct LamC ([params : (Listof Symbol)] [body : ExprC])  #:transparent)

;define Environment
(define-type Env (Listof Binding))
(struct Binding ((id : Symbol) (val : Value)) #:transparent)

;define Value
(define-type Value (U NumV PrimV CloV))
(struct NumV ([n : Real]) #:transparent)
;(struct FunV ([params : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct PrimV ([fun : (-> (Listof Value) Value)]) #:transparent)
(struct CloV ([params : (Listof Symbol)] [body : ExprC] [clo-env : Env]) #:transparent)

;-------------------------------------------------------------------------------------------

;;top-interp
;;Given an SExpression, fun-sexps returns a Real by evaluating the expression
;Input: Sexp / Output: Real
(define (top-interp [sexps : Sexp] [topEnv : Env]) : Value
 (interp (parse sexps) topEnv))

;-----------------------------------------------------------------------------------------

;interp
;;Given an ExprC and a list of FunDefC structrs, interp evaluates the expression and returns a real
;Input: ExprC/List of FunDefC structs / Output: Real
(define (interp [a : ExprC] [env : Env]) : Value
   (match a
     
     [(NumC n) (NumV n)]
     
     [(IdC s) (env-lookup s env)]

     [(LamC param body) (CloV param body env)]
     
     [(AppC fun args)
      
      ;evaluate expression that should return a function value
      (define funval (interp fun env))
      
      (match funval
        
        [(PrimV fun) (fun (map (λ ([x : ExprC]) (interp x env)) args))]

        [(CloV params body clo-env)

         ;evaluate arguments (eager)
         (define arg_vals (map (λ ([x : ExprC]) (interp x env)) args))

         ;extend CloV environment with parameters
         (define new-env (env-extend-all params arg_vals clo-env))
         
         ;evaluate the function body
         (interp body new-env)]
        
        [else (error 'interp "invalid function call")])]))


;-------------------------------------------------------------------------------------------
;define Arithmetic functions

;;adds two NumV's
(define (add [args : (Listof Value)]) : Value
  (match args
    [(list (? NumV? a) (? NumV? b)) (NumV (+ (NumV-n a) (NumV-n b)))]
    [else (error '+ "DXUQ invalid arguments passed to +")]))

;;subtracts two NumV's
(define (subtract [args : (Listof Value)]) : Value
  (match args
    [(list (? NumV? a) (? NumV? b)) (NumV (- (NumV-n a) (NumV-n b)))]
    [else (error '+ "DXUQ invalid arguments passed to -")]))

;;multiply two NumV's
(define (multiply [args : (Listof Value)]) : Value
  (match args
    [(list (? NumV? a) (? NumV? b)) (NumV (* (NumV-n a) (NumV-n b)))]
    [else (error '+ "DXUQ invalid arguments passed to *")]))

;;divide two NumV's
(define (divide [args : (Listof Value)]) : Value
  (match args
    [(list (? NumV? a) (? NumV? b)) (NumV (/ (NumV-n a) (NumV-n b)))]
    [else (error '+ "DXUQ invalid arguments passed to /")]))


;-------------------------------------------------------------------------------------------

;;grabs the value of an id in a given environment
(define (env-lookup [id : Symbol] [env : Env]) : Value
  (cond
    [(empty? env)
     (error 'env-lookup (string-append "DXUQ unbound identifier: " (symbol->string id)))]
    [(equal? id (Binding-id (first env))) (Binding-val (first env))]
    [else (env-lookup id (rest env))]))

;-------------------------------------------------------------------------------------------

;;adds a list of parameters with a list of arguments to an env
(define (env-extend-all [params : (Listof Symbol)] [args : (Listof Value)] [env : Env]) : Env
  (cond
    [(empty? params) env]
    [else (env-extend-all
           (rest params)
           (rest args)
           (env-extend (first params) (first args) env))]))

;-------------------------------------------------------------------------------------------

;;adds a binding to an environment
(define (env-extend [param : Symbol] [arg : Value] [env : Env]) : Env
  (cons (Binding param arg) env))

;-----------------------------------------------------------------------------------------

;parse
;Given an SExpression, parse returns an ExprC struct representing a parsed version of the SExpression
;Input: Sexp / Output: ExprC Struct
(define (parse [s : Sexp]) : ExprC
 (match s
   [(? real? a) (NumC a)]
   [(? symbol? id) (cond [(valid-idc id) (IdC id)]
                         [else (error 'parse (string-append
                                              "DXUQ invalid identifier: "
                                              (symbol->string id)))])]
   [(list 'fn (list (? symbol? params) ...) body) (LamC (cast params (Listof Symbol)) (parse body))]
   
   [(list 'let defs ... 'in body) (desugar-let (cast defs (Listof Sexp)) body)]
   
   [(list fun args ...) (AppC (parse fun) (map parse args))]
   [else (error 'parse "DXUQ invalid input to parse")]))


;desugars a let into an AppC
(define (desugar-let [defs : (Listof Sexp)] [body : Sexp]) : AppC
  (AppC (LamC (map get-var-name defs) (parse body))
        (map (λ ([x : Sexp]) (parse (get-var-value x))) defs)))

;parse a definition, return variable name
(define (get-var-name [def : Sexp]) : Symbol
  (match def
    [(list (? symbol? name) '= value) name]
    [else (error (string-append "DXUQ invalid variable definition"))]))

;parse a definition, return variable name
(define (get-var-value [def : Sexp]) : Sexp
  (match def
    [(list (? symbol? name) '= value) value]
    [else (error (string-append "DXUQ invalid variable definition"))]))


;-----------------------------------------------------------------------------------------

;valid-idc
;Given a symbol, valid-idc returns a boolean representing whether the symbol is a valid argument to BinOp
;Input: Symbol / Output: Boolean
(define (valid-idc [id : Sexp]) : Boolean
  (match id
    ['let #f]
    ['in #f]
    ['if #f]
    ['fn #f]
    [else #t]))


;;----------------------------------------------------------------------------------------------

;define top-env for test cases
(define top-env (list (Binding '+ (PrimV add))
                      (Binding '* (PrimV multiply))
                      (Binding '/ (PrimV divide))
                      (Binding '- (PrimV subtract))))

;;----------------------------------------------------------------------------------------------
;Test Cases

;Test Cases for the top-interp
(check-equal? (top-interp '{+ 4 5} top-env) (NumV 9))
(check-equal? (top-interp '{{fn {x y} {* x y}} 3 7} top-env) (NumV 21))
(check-equal? (top-interp '{{fn {x add4} {add4 x}} 20 {fn {x} {+ x 4}}} top-env) (NumV 24))
(check-equal? (top-interp
               '{{fn {add1 dotwice} {dotwice add1 14}} {fn {x} {+ x 1}} {fn {f a} {f {f a}}}}
               top-env) (NumV 16))
(check-equal? (top-interp '{{fn {y} {{fn {fun y} {fun}} {fn {} y} 100}} 1} top-env) (NumV 1))
(check-equal? (top-interp '{let {x = 10} {y = 2} in {* x y}} top-env) (NumV 20))
(check-equal? (top-interp '{let {x = {/ 144 12}} {y = {* 2 3}} in {* x y}} top-env) (NumV 72))


;Test Cases for the interp
(check-equal? (interp (AppC (IdC '+) (list (NumC 4) (NumC 5))) top-env) (NumV 9))
(check-equal? (interp (AppC (IdC '-) (list (NumC 10) (NumC 5))) top-env) (NumV 5))
(check-equal? (interp (AppC (IdC '*) (list (NumC 10) (NumC 5))) top-env) (NumV 50))
(check-equal? (interp (AppC
                       (LamC (list 'x) (AppC (IdC '+) (list (IdC 'x) (NumC 1)))) (list (NumC 5)))
                      top-env) (NumV 6))
(check-equal? (interp(AppC
                      (LamC (list 'x 'y)
                            (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))) (list (NumC 25)(NumC 5)))
                      top-env) (NumV 30))
(check-exn (regexp (regexp-quote "invalid function call"))
          (lambda () (interp (AppC (NumC 5) (list (NumC 4) (NumC 5))) top-env)))

;Test cases for env-lookup
(check-equal? (env-lookup 'var (list (Binding 'var (NumV 4)))) (NumV 4))
(check-exn (regexp (regexp-quote "DXUQ unbound identifier: x"))
          (lambda () (env-lookup 'x (list (Binding 'y (NumV 4))))))

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

;Test Cases for the parse
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
(check-exn (regexp (regexp-quote "DXUQ invalid input to parse"))
          (lambda () (parse '{})))

;Test Cases for desugar-let
(check-equal? (desugar-let (list '(z = 4)) '(+ z 1))
              (AppC (LamC '(z) (AppC (IdC '+) (list (IdC 'z) (NumC 1))))
                    (list (NumC 4))))

;Test Cases for get-var-name
(check-equal? (get-var-name '(var = 45)) 'var)
(check-exn (regexp (regexp-quote "DXUQ invalid variable definition"))
          (lambda () (get-var-name '(var == 45))))

;Test Cases for get-var-value
(check-equal? (get-var-value '(var = 45)) '45)
(check-equal? (get-var-value '(var = {* 4 5})) '(* 4 5))
(check-exn (regexp (regexp-quote "DXUQ invalid variable definition"))
          (lambda () (get-var-value '(var == 45))))

;Test Cases for the valid
(check-equal? (valid-idc 'x) #t)
(check-equal? (valid-idc '+) #t)
(check-equal? (valid-idc 'if) #f)
(check-equal? (valid-idc 'fn) #f)
(check-equal? (valid-idc 'let) #f)
(check-equal? (valid-idc 'in) #f)

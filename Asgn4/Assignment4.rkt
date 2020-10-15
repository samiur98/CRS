;Assignment 4: DXUQ4 Parser and Interpreter with multiple arguments.

;The project has been finished. All functions have been implemented and they
;have been tested by a suite of test cases that test all main functions as well
;as helper functions.

#lang typed/racket

(require typed/rackunit)

(define EPSILON 1e-05)

;-------------------------------------------------------------------------------------------
; Data Definitions:
;   ExprC, FunDefC

;Creating the data-types of both the ExprC and the FunDefC
(define-type ExprC (U NumC IdC AppC LamC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct LamC ([param : (Listof Symbol)] [body : ExprC])  #:transparent)

;define Environment
(define-type Env (Listof Binding))
(struct Binding ((id : Symbol) (val : Value)) #:transparent)

;define Value
(define-type Value (U FunV NumV PrimV))
(struct NumV ([n : Real]) #:transparent)
(struct FunV ([params : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct PrimV ([s : Symbol]) #:transparent)

;define top-env
(define top-env (list (Binding '+ (PrimV '+))
                      (Binding '* (PrimV '*))
                      (Binding '/ (PrimV '/))
                      (Binding '- (PrimV '-))))

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

     [(LamC param body) (FunV param body)]
     
     [(AppC fun args)
      
      ;evaluate expression that should return a function value
      (define funval (interp fun env))
      
      (match funval
        [(PrimV s) (eval-prim s (map (λ ([x : ExprC]) (interp x env)) args))]
        [(FunV params body)
         (interp body
          (env-extend-all params (map (λ ([x : ExprC]) (interp x env)) args) env))]
        [else (error 'interp "invalid function call")])]))

;-------------------------------------------------------------------------------------------

;evaluates a primitive function to a NumV
(define (eval-prim [s : Symbol] [args : (Listof Value)]) : Value
  (match args
    [(list (? NumV? a) (? NumV? b))
     (NumV ((symbol->arith s) (NumV-n a) (NumV-n b)))]
    [else (error 'eval-prim (string-append "invalid arguments passed to "
                                           (symbol->string s)))]))

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

;symbol->arith
;Function that serves as a Lookup table where the input which is a symbol is mapped to an arithmetic function.
;Input: Symbol / Output: Operator
(define (symbol->arith [s : Symbol]) : (-> Real Real Real)
  (match s
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]
    [else (error 'symbol->arith "DXUQ invalid input")]))

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
   [(list fun args ...) (AppC (parse fun) (map parse args))]
   [else (error 'parse "DXUQ invalid input to parse")]))

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
;;----------------------------------------------------------------------------------------------
;Test Cases

;Test Cases for the top-interp


;Test Cases for the interp-fns


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

;Test cases for eval-prim
(check-equal? (eval-prim '+ (list (NumV 5) (NumV 6))) (NumV 11))
(check-equal? (eval-prim '/ (list (NumV 100) (NumV 5))) (NumV 20))
(check-equal? (eval-prim '- (list (NumV 100) (NumV 5))) (NumV 95))
(check-equal? (eval-prim '* (list (NumV 100) (NumV 5))) (NumV 500))
(check-exn (regexp (regexp-quote "invalid arguments passed to +"))
          (lambda () (eval-prim '+ (list (NumV 5) (NumV 6) (NumV 5)))))
(check-exn (regexp (regexp-quote "invalid arguments passed to -"))
          (lambda () (eval-prim '- (list (PrimV '+) (NumV 6) (NumV 5)))))


;Test Cases for the symbol->arith
(check-equal? (symbol->arith '+) +)
(check-equal? (symbol->arith '-) -)
(check-equal? (symbol->arith '/) /)
(check-equal? (symbol->arith '*) *)
(check-exn (regexp (regexp-quote "DXUQ invalid input"))
          (lambda () (symbol->arith '%)))


;Test Cases for the parse
(check-equal? (parse '{+ 1 2}) (AppC (IdC '+) (list (NumC 1) (NumC 2))))
(check-equal? (parse '{fn {x y} {* x y}})
              (LamC (list 'x 'y) (AppC (IdC '*) (list (IdC 'x) (IdC 'y)))))
(check-equal? (parse '{{fn {x y} {* x y}} 2 16})
              (AppC (LamC (list 'x 'y) (AppC (IdC '*) (list (IdC 'x) (IdC 'y))))
                    (list (NumC 2) (NumC 16))))
(check-exn (regexp (regexp-quote "DXUQ invalid identifier: in"))
          (lambda () (parse '{{fn {in y} {* in y}} 2 16})))
(check-exn (regexp (regexp-quote "DXUQ invalid input to parse"))
          (lambda () (parse '{})))

;Test Cases for the valid
(check-equal? (valid-idc 'x) #t)
(check-equal? (valid-idc '+) #t)
(check-equal? (valid-idc 'if) #f)
(check-equal? (valid-idc 'fn) #f)
(check-equal? (valid-idc 'let) #f)
(check-equal? (valid-idc 'in) #f)

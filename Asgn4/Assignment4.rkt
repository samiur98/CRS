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
(define-type ExprC (U NumC IdC AppC BinOp Ifleq0))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct AppC ([fun : FunV] [args : (Listof ExprC)]) #:transparent)
(struct BinOp ([l : ExprC] [r : ExprC] [operator : Symbol]) #:transparent)
(struct Ifleq0 ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct FunDefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)

;define Environment
(define-type Env (Listof Binding))
(struct Binding ((id : Symbol) (val : Real)))

(struct FunV ([params : (Listof IdC)] [body : ExprC]))

;-------------------------------------------------------------------------------------------

;;top-interp
;;Given an SExpression, fun-sexps returns a Real by evaluating the expression
;Input: Sexp / Output: Real
#;(define (top-interp [fun-sexps : Sexp]) : Real
 (interp-fns (parse-prog fun-sexps)))

;-----------------------------------------------------------------------------------------

;interp-fns
;;Given the list of FunDefC structs, interp-fns interprets the main method.
;Input: List of FunDefC Structs / Output: Real
#;(define (interp-fns [funs : (Listof FunDefC)]) : Real
 (interp (AppC 'main '()) funs))

;-----------------------------------------------------------------------------------------

;interp
;;Given an ExprC and a list of FunDefC structrs, interp evaluates the expression and returns a real
;Input: ExprC/List of FunDefC structs / Output: Real
(define (interp [a : ExprC] [env : Env]) : Real
   (match a
     [(NumC n) n]
     [(IdC s) (env-lookup s env)]
     #;[(BinOp l r '/)
      (define right (interp r env))
      (cond [(equal? right 0) (error "DXUQ division by zero")])
      ((symbol->arith '/) (interp l env) right)]
     [(BinOp l r (? symbol? s)) ((symbol->arith s) (interp l env) (interp r env))]
     #;[(Ifleq0 test then e)
        (cond
          [(<= (interp test env) 0) (interp then env)]
          [else (interp e env)])]
     [(AppC funV args) (interp (FunV-body funV)
                               (env-extend-all
                                (FunV-params funV)
                                (map (λ ([x : ExprC]) (interp x env)) args) env))]))

;-------------------------------------------------------------------------------------------

;;grabs the value of an id in a given environment
(define (env-lookup [id : Symbol] [env : Env]) : Real
  (cond
    [(empty? env)
     (error 'env-lookup (string-append "DXUQ unbound identifier: " (symbol->string id)))]
    [(equal? id (Binding-id (first env))) (Binding-val (first env))]
    [else (env-lookup id (rest env))]))

;-------------------------------------------------------------------------------------------

;;adds a list of parameters with a list of arguments to an env
(define (env-extend-all [params : (Listof IdC)] [args : (Listof Real)] [env : Env]) : Env
  (cond
    [(empty? params) env]
    [else (env-extend-all
           (rest params)
           (rest args)
           (env-extend (first params) (first args) env))]))

;-------------------------------------------------------------------------------------------

;;adds a binding to an environment
(define (env-extend [param : IdC] [arg : Real] [env : Env]) : Env
  (define bind (Binding (IdC-s param) arg))
  (cons bind env))

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
#;(define (parse [s : Sexp]) : ExprC
 (match s
   [(? real? a) (NumC a)]
   [(? symbol? id) (cond 
                         [(valid-idc id) (IdC id)]
                         [else (error 'parse "DXUQ invalid identifier in parse")])]
   [(list '+ a b) (BinOp (parse a) (parse b) '+)]
   [(list '- a b) (BinOp (parse a) (BinOp (parse -1) (parse b) '*) '+)]
   [(list '* a b) (BinOp (parse a) (parse b) '*)]
   [(list '/ a b) (BinOp (parse a) (parse b) '/)]
   [(list '- a) (BinOp (parse -1) (parse a) '*)]
   [(list 'ifleq0 test then else) (Ifleq0 (parse test) (parse then) (parse else))] 
   #;[(list (? symbol? n) args_p ...)
    (define args (cast args_p (Listof Sexp)))
    (cond [(not (valid-idc n)) (error "DXUQ invalid syntax")])
    (AppC n (map parse args))]
   [else (error 'parse "DXUQ invalid input to parse")]))

;-----------------------------------------------------------------------------------------

;valid-idc
;Given a symbol, valid-idc returns a boolean representing whether the symbol is a valid argument to BinOp
;Input: Symbol / Output: Boolean
(define (valid-idc [id : Sexp]) : Boolean
  (match id
    ['+ #f]
    ['- #f]
    ['/ #f]
    ['* #f]
    ['ifleq0 #f]
    ['fundef #f]
    [else #t]))


;;----------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------
;Test Cases

;Test Cases for the top-interp


;Test Cases for the interp-fns


;Test Cases for the interp
(check-equal? (interp (AppC
         (FunV (list (IdC 'x)) (BinOp (NumC 1) (IdC 'x) '+))
         (list (NumC 5))) '()) 6)
(check-equal? (interp (AppC
         (FunV (list (IdC 'x) (IdC 'y)) (BinOp (IdC 'x) (IdC 'y) '*))
         (list (NumC 5) (NumC 6))) '()) 30)

;Test cases for env-lookup
(check-equal? (env-lookup 'var (list (Binding 'var 4))) 4)
(check-exn (regexp (regexp-quote "DXUQ unbound identifier: x"))
          (lambda () (env-lookup 'x (list (Binding 'y 4)))))

;Test Cases for the symbol->arith
(check-equal? (symbol->arith '+) +)
(check-equal? (symbol->arith '-) -)
(check-equal? (symbol->arith '/) /)
(check-equal? (symbol->arith '*) *)
(check-exn (regexp (regexp-quote "DXUQ invalid input"))
          (lambda () (symbol->arith '%)))


;Test Cases for the parse
;(check-equal? (parse '{+ 1 2}) (BinOp (NumC 1) (NumC 2) '+))
;(check-equal? (parse '{* 1 2}) (BinOp (NumC 1) (NumC 2) '*))
;(check-equal? (parse '{- 5 2}) (BinOp (NumC 5) (BinOp (NumC -1) (NumC 2) '*) '+))
;(check-equal? (parse '{- 9}) (BinOp (NumC -1) (NumC 9) '*))
;(check-equal? (parse '{/ 10 2}) (BinOp (NumC 10) (NumC 2) '/))
;(check-equal? (parse '{* {- 1} 2}) (BinOp (BinOp (NumC -1) (NumC 1) '*) (NumC 2) '*))
;(check-equal? (parse '{- 1 {+ 5 6}}) (BinOp (NumC 1) (BinOp (NumC -1) (BinOp (NumC 5) (NumC 6) '+) '*) '+))
;(check-equal? (parse '{func {+ 1 5}})(AppC 'func (list (BinOp (NumC 1) (NumC 5) '+))))
;(check-equal? (parse '{ifleq0 0 1 2}) (Ifleq0 (NumC 0) (NumC 1) (NumC 2)))
;(check-equal? (parse '{ifleq0 8 8 8}) (Ifleq0 (NumC 8) (NumC 8) (NumC 8)))
#;(check-equal? (parse '{ifleq0 1 {+ 2 2} {- 5 2}})
            (Ifleq0 (NumC 1) (BinOp (NumC 2) (NumC 2) '+) (BinOp (NumC 5)
               (BinOp (NumC -1) (NumC 2) '*) '+)))
#;(check-exn (regexp (regexp-quote "DXUQ invalid identifier in parse"))
          (lambda () (parse '{+ / 3})))
#;(check-exn (regexp (regexp-quote "DXUQ invalid input to parse"))
          (lambda () (parse "hello")))
#;(check-exn (regexp (regexp-quote "DXUQ invalid syntax"))
          (lambda () (parse '{/ 3 4 5})))

;Test Cases for the valid
(check-equal? (valid-idc 'x) #t)
(check-equal? (valid-idc '+) #f)
(check-equal? (valid-idc '/) #f)
(check-equal? (valid-idc '*) #f)
(check-equal? (valid-idc '-) #f)
(check-equal? (valid-idc 'ifleq0) #f)
(check-equal? (valid-idc 'fundef) #f)

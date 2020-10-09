;Assignment 2: DXUQ2 Parser and Interpreter
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
(struct AppC ([fun : Symbol] [args : (Listof ExprC)]) #:transparent)
(struct BinOp ([l : ExprC] [r : ExprC] [operator : Symbol]) #:transparent)
(struct Ifleq0 ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)

(struct FunDefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)

;;---------------------------------------------------------------------------------------------

; top-interp
;;Given an SExpression, fun-sexps returns a Real by evaluating the expression
;Input: Sexp / Output: Real
(define (top-interp [fun-sexps : Sexp]) : Real
 (interp-fns (parse-prog fun-sexps)))

;;---------------------------------------------------------------------------------------------

;interp-fns
;;Given the list of FunDefC structs, interp-fns interprets the main method.
;Input: List of FunDefC Structs / Output: Real
(define (interp-fns [funs : (Listof FunDefC)]) : Real
 (interp (AppC 'main '()) funs))

;;--------------------------------------------------------------------------------------------------

;interp
;;Given an ExprC and a list of FunDefC structrs, interp evaluates the expression and returns a real
;Input: ExprC/List of FunDefC structs / Output: Real
(define (interp [a : ExprC] [fds : (Listof FunDefC)]) : Real
   (match a
     [(NumC n) n]
     [(BinOp l r (? symbol? s))
      (define right (interp r fds))
      (cond [(and (equal? right 0) (equal? s '/)) (error "DXUQ division by zero")])
      ((symbol->arith s) (interp l fds) right)]
     [(Ifleq0 test then e)
        (cond
          [(<= (interp test fds) 0) (interp then fds)]
          [else (interp e fds)])]
     [(IdC s) (error 'interp "DXUQ undefined identifier")]
     [(AppC f args) (define fd (get-fundef f fds))
                    (cond [(not (equal? (length args) (length (FunDefC-args fd))))
                           (error f "DXUQ wrong number of arguments provided")])
                    (interp (NumC (interp
                                   (subst-args args (FunDefC-args fd) (FunDefC-body fd)) fds))fds)]))

;;--------------------------------------------------------------------------------------------------

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

;;--------------------------------------------------------------------------------------------------

;subst-args
;;calls subst for each argument in 'what', handling multi-arg functions
;Input: List of ExprC/ListOf Symbol/ExrC
(define (subst-args [args : (Listof ExprC)] [params : (Listof Symbol)] [in : ExprC]) : ExprC
  (cond
    [(empty? args) in]
    [else (subst-args (rest args) (rest params) (subst (first args) (first params) in))]))

;;---------------------------------------------------------------------------------------------------

;subst
;Given an ExprC, a symbol and another ExprC, subst returns an ExprC struct substituting the symbol
;for the second ExprC struct
;Input: ExprC/Symbol/ExprC / Output: ExprC Struct
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
 (match in
   [(NumC n) in]
   [(IdC s) (cond
              [(symbol=? s for) what]
              [else in])]
   [(AppC f args) (AppC f (map (λ ([arg : ExprC]) (subst what for arg)) args))]
   [(BinOp l r '+) (BinOp (subst what for l)
                       (subst what for r) '+)]
   [(BinOp l r '*) (BinOp (subst what for l)
                       (subst what for r) '*)]
   [(BinOp l r '/) (BinOp (subst what for l)
                       (subst what for r) '/)]
   [(Ifleq0 test then else) (Ifleq0
                            (subst what for test)
                            (subst what for then)
                            (subst what for else))]))

;;--------------------------------------------------------------------------------------------------

;parse-prog
;;Given an SExpression, parse-prog returns a List of FunDefC structs representing a parsed version of FundefC structs.
;Input: Sexp / Output: List of FunDefC Structs
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
 (match s
   [(list funs ...) (map parse-fundef funs)]))

;;---------------------------------------------------------------------------------------------------

;parse-fundef
;Given a SExpression, parse-fundef returns a FunDefC struct representing the parsed version of the SExpression
;Input: Sexp / Output: FunDefC Struct
(define (parse-fundef [s : Sexp]) : FunDefC
 (match s
   [(list 'fundef (list (? symbol? name) (? symbol? args_t) ...) body)
    (define args (cast args_t (Listof Symbol)))
    (cond [(check-duplicates args) (error "DXUQ invalid syntax")])
     (FunDefC name args (parse body))]
   [else (error 'parse-fundef "DXUQ invalid input to parse-fundef")]))

;;---------------------------------------------------------------------------------------------------

;Parse
;Given an SExpression, parse returns an ExprC struct representing a parsed version of the SExpression
;Input: Sexp / Output: ExprC Struct
(define (parse [s : Sexp]) : ExprC
 (match s
   [(? real? a) (NumC a)]
   [(? symbol? id) (cond 
                         [(valid-idc id) (IdC id)]
                         [else (error 'parse "DXUQ invalid identifier in parse")])]
   [(list '+ a b) (BinOp (parse a) (parse b) '+)]
   [(list '- a b) (BinOp (parse a) (BinOp (parse -1) (parse b) '*) '+)]
   [(list '* a b) (BinOp (parse a) (parse b) '*)]
   [(list '/ a b) (cond
                    [#f (error 'parse "DXUQ cannot divide by zero")]
                    [else (BinOp (parse a) (parse b) '/)])]
   [(list '- a) (BinOp (parse -1) (parse a) '*)]
   [(list 'ifleq0 test then else) (Ifleq0 (parse test) (parse then) (parse else))] 
   [(list (? symbol? n) args_p ...)
    (define args (cast args_p (Listof Sexp)))
    (cond [(not (valid-idc n)) (error "DXUQ invalid syntax")])
    (AppC n (map parse args))]
   [else (error 'parse "DXUQ invalid input to parse")]))

;;---------------------------------------------------------------------------------------------------

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

;;---------------------------------------------------------------------------------------------------

;get-fundef
;Given an input of a symbol and a list of FunDefC structs, get-fundef returns a single funDefC
;struct that matches with the symbol passed in.
;Input: Symbol/List of FundefC structs / Output: FunDefC struct
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
   (cond
     [(empty? fds)
      (error 'get-fundef "DXUQ reference to undefined function")]
     [(cons? fds)
      (cond
        [(equal? n (FunDefC-name (first fds))) (first fds)]
        [else (get-fundef n (rest fds))])]))


;;----------------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------

;Test Cases

;Test Cases for the top-interp function
(check-equal? (top-interp '{{fundef {f x} {+ x 2}}
                          {fundef {main} {f 1}}}) 3)
(check-equal? (top-interp '{{fundef {f p} {* {- 10 5} p}}
                          {fundef {main} {+ {f 2} {f 3}}}})25)
(check-equal? (top-interp '{{fundef {u} {- 5}}
                          {fundef {f x y} {* x y}}
                          {fundef {main} {+ {f 6 5} {u}}}})25)
(check-equal? (top-interp '{{fundef {f x y} {ifleq0 {- x y} x y}}
                            {fundef {main} {f 6 5}}})5)
(check-equal? (top-interp '{{fundef {main} {ifleq0 {* 3 1} 3 {+ 2 9}}}}) 11)
(check-exn (regexp (regexp-quote "DXUQ wrong number of arguments provided"))
          (lambda () (top-interp '{{fundef {f p} {* {- 10 5} p}}
                          {fundef {main} {+ {f 2 3} {f 3}}}})))
(check-exn (regexp (regexp-quote "DXUQ invalid syntax"))
          (lambda () (top-interp '{{fundef {f p p} {* p p}}
                            {fundef {main} {f 1 6}}})))

;Test Cases for the interp-fns function
(check-equal? (interp-fns
              (list (FunDefC 'func '{x} (NumC 4)) (FunDefC 'main '{} (NumC 5))))5)
(check-equal? (interp-fns
      (parse-prog '{{fundef {f x} {+ x 5}}
                    {fundef {main} {f 1}}})) 6)
(check-equal? (interp-fns
       (parse-prog '{{fundef {f var} {* var 2}}
                     {fundef {main} {+ {f 4} {f 5}}}}))18)

;Test Cases for the interp function
(check-= (interp (BinOp (NumC 10) (NumC 2) '/) '()) 5 EPSILON)
(check-= (interp (BinOp (NumC 4) (NumC 5) '+) '()) 9 EPSILON)
(check-= (interp (BinOp (NumC 4) (NumC 5) '*) '()) 20 EPSILON)
(check-= (interp (BinOp (BinOp (NumC 3) (NumC 2) '*) (NumC 5) '+) '()) 11 EPSILON)
(check-= (interp (AppC 'func (list (NumC 10)))
               (list (FunDefC 'func '{x} (BinOp (IdC 'x) (NumC 1) '+))))
         11 EPSILON)
(check-= (interp (Ifleq0 (NumC 0) (NumC 1) (NumC 2)) '()) 1 EPSILON)
(check-= (interp (Ifleq0 (NumC -1) (NumC 1) (NumC 2)) '()) 1 EPSILON)
(check-= (interp (Ifleq0 (NumC 1) (NumC 1) (NumC 2)) '()) 2 EPSILON)
(check-exn (regexp (regexp-quote "DXUQ undefined identifier"))
          (lambda () (interp (BinOp (IdC 'z) (NumC 1) '+) '())))

;Test Cases for the symbol->arith function
(check-equal? (symbol->arith '+) +)
(check-equal? (symbol->arith '-) -)
(check-equal? (symbol->arith '/) /)
(check-equal? (symbol->arith '*) *)
(check-exn (regexp (regexp-quote "DXUQ invalid input"))
          (lambda () (symbol->arith '%)))

;Test Cases for the subst-args function
(check-equal? (subst-args (list (NumC 4) (NumC 8)) '(x y) (BinOp (IdC 'x) (IdC 'y) '+))
              (BinOp (NumC 4) (NumC 8) '+))

;Test Cases for the subst function
(check-equal? (subst (NumC 4) 'x (BinOp (IdC 'x) (NumC 1) '+))
            (BinOp (NumC 4) (NumC 1) '+))
(check-equal? (subst (NumC 4) 'x (BinOp (IdC 'x) (NumC 2) '/))
            (BinOp (NumC 4) (NumC 2) '/))
(check-equal? (subst (NumC 4) 'x (AppC 'func (list (BinOp (IdC 'x) (NumC 10) '+))))
            (AppC 'func (list (BinOp (NumC 4) (NumC 10) '+))))
(check-equal? (subst (NumC 4) 'x (Ifleq0 (IdC 'x) (NumC 2) (NumC 3)))
            (Ifleq0 (NumC 4) (NumC 2) (NumC 3)))

;Test Cases for the parse-prog function
(check-equal? (parse-prog '{})'())
(check-equal? (parse-prog '{{fundef {myfunc x} {+ 1 x}}})
            (list (FunDefC 'myfunc '{x} (BinOp (NumC 1) (IdC 'x) '+))))

;Test Cases for the parse-fundef function
(check-equal? (parse-fundef '{fundef {myfunc x} {+ 1 x}})
            (FunDefC 'myfunc '{x} (BinOp (NumC 1) (IdC 'x) '+)))
(check-exn (regexp (regexp-quote "DXUQ invalid input to parse-fundef"))
          (lambda () (parse-fundef '(+ 1 4))))

;Test Cases for the parse function
(check-equal? (parse '{+ 1 2}) (BinOp (NumC 1) (NumC 2) '+))
(check-equal? (parse '{* 1 2}) (BinOp (NumC 1) (NumC 2) '*))
(check-equal? (parse '{- 5 2}) (BinOp (NumC 5) (BinOp (NumC -1) (NumC 2) '*) '+))
(check-equal? (parse '{- 9}) (BinOp (NumC -1) (NumC 9) '*))
(check-equal? (parse '{/ 10 2}) (BinOp (NumC 10) (NumC 2) '/))
(check-equal? (parse '{* {- 1} 2}) (BinOp (BinOp (NumC -1) (NumC 1) '*) (NumC 2) '*))
(check-equal? (parse '{- 1 {+ 5 6}}) (BinOp (NumC 1) (BinOp (NumC -1) (BinOp (NumC 5) (NumC 6) '+) '*) '+))
(check-equal? (parse '{func {+ 1 5}})(AppC 'func (list (BinOp (NumC 1) (NumC 5) '+))))
(check-equal? (parse '{ifleq0 0 1 2}) (Ifleq0 (NumC 0) (NumC 1) (NumC 2)))
(check-equal? (parse '{ifleq0 8 8 8}) (Ifleq0 (NumC 8) (NumC 8) (NumC 8)))
(check-equal? (parse '{ifleq0 1 {+ 2 2} {- 5 2}})
            (Ifleq0 (NumC 1) (BinOp (NumC 2) (NumC 2) '+) (BinOp (NumC 5)
               (BinOp (NumC -1) (NumC 2) '*) '+)))
(check-exn (regexp (regexp-quote "DXUQ invalid identifier in parse"))
          (lambda () (parse '{+ / 3})))
(check-exn (regexp (regexp-quote "DXUQ invalid input to parse"))
          (lambda () (parse "hello")))
(check-exn (regexp (regexp-quote "DXUQ cannot divide by zero"))
          (lambda () (parse '{/ 8 0})))
(check-exn (regexp (regexp-quote "DXUQ invalid syntax"))
          (lambda () (parse '{/ 3 4 5})))

;Test Cases for the valid Function
(check-equal? (valid-idc 'x) #t)
(check-equal? (valid-idc '+) #f)
(check-equal? (valid-idc '/) #f)
(check-equal? (valid-idc '*) #f)
(check-equal? (valid-idc '-) #f)
(check-equal? (valid-idc 'ifleq0) #f)
(check-equal? (valid-idc 'fundef) #f)

;Test Cases for the get-fundef function
(check-equal? (get-fundef 'func1 (list (FunDefC 'func2 '{x} (NumC 4))
                                      (FunDefC 'func1 '{x} (NumC 5))))
            (FunDefC 'func1 '{x} (NumC 5)))
(check-exn (regexp (regexp-quote "get-fundef: DXUQ reference to undefined function"))
          (lambda () (get-fundef 'func '())))
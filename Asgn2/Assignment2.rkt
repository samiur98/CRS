#lang typed/racket

(require typed/rackunit)

(define EPSILON 1e-05)

;-------------------------------------------------------------------------------------------
; Data Definitions:
;   ExprC, FunDefC

(define-type ExprC (U NumC PlusC MultC IdC))
(struct NumC ([n : Real]) #:transparent)
(struct PlusC ([l : ExprC] [r : ExprC]) #:transparent)
(struct MultC ([l : ExprC] [r : ExprC]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)

(struct FunDefC ([name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent)

;-------------------------------------------------------------------------------------------
; Parse Functions

;;maps an s-expression directly to an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? a) (NumC a)]
    [(? symbol? id) (IdC id)]
    [(list '+ a b) (PlusC (parse a) (parse b))]
    [(list '* a b) (MultC (parse a) (parse b))]
    [(list '- a b) (PlusC (parse a) (MultC (NumC -1)(parse b)))]
    [(list '- a) (MultC (NumC -1) (parse a))]
    [else (error 'parse "invalid input to parse")]))

(check-equal? (parse '(+ 1 2))
              (PlusC (NumC 1) (NumC 2)))
(check-equal? (parse '(+ 1 var))
              (PlusC (NumC 1) (IdC 'var)))
(check-equal? (parse '(* (- 1) 2))
              (MultC (MultC (NumC -1) (NumC 1)) (NumC 2)))
(check-equal? (parse '(- 1 (+ 5 6)))
              (PlusC (NumC 1) (MultC (NumC -1) (PlusC (NumC 5) (NumC 6)))))
(check-exn (regexp (regexp-quote "invalid input to parse"))
           (lambda () (parse "hello")))

;;parses a function definition, in an s-expression, into a FunDefC
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list (? symbol? name) (? symbol? arg) (? list? body))
     (FunDefC name arg (parse body))]
    [else (error 'parse "invalid input to parse-fundef")]))

(check-equal? (parse-fundef '(myfunc x (+ 1 x)))
              (FunDefC 'myfunc 'x (PlusC (NumC 1) (IdC 'x))))
(check-exn (regexp (regexp-quote "invalid input to parse-fundef"))
           (lambda () (parse-fundef '(+ 1 4))))

;-------------------------------------------------------------------------------------------
; interp

;;evaluates an ExprC to a value
(define (interp [a : ExprC]) : Real
    (match a
      [(NumC n) n]
      [(PlusC l r) (+ (interp l) (interp r))]
      [(MultC  l r) (* (interp l) (interp r))]))

(check-= (interp (PlusC (NumC 4) (NumC 5))) 9 EPSILON)
(check-= (interp (MultC (NumC 4) (NumC 5))) 20 EPSILON)
(check-= (interp (PlusC (MultC (NumC 3) (NumC 2)) (NumC 5))) 11 EPSILON)


;-------------------------------------------------------------------------------------------
; top-interp

;;interperates an S-expression
(define (top-interp [s : Sexp]) : Real
  (interp (parse s)))

(check-equal? (top-interp '{+ 2 3}) 5)
(check-equal? (top-interp '{* 2 {- {- {- 5 2}}}}) 6)
(check-equal? (top-interp '{+ {* 4 {- 2}} {- 10 5}}) -3)










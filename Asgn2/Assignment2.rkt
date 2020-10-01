#lang typed/racket

(require typed/rackunit)

(define EPSILON 1e-05)

;-------------------------------------------------------------------------------------------
; Data Definitions:
;   ExprC, FunDefC

(define-type ExprC (U NumC PlusC MultC IdC AppC))
(struct NumC ([n : Real]) #:transparent)
(struct PlusC ([l : ExprC] [r : ExprC]) #:transparent)
(struct MultC ([l : ExprC] [r : ExprC]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct AppC ([fun : Symbol] [arg : ExprC]) #:transparent)

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
    [(list (? symbol? n) (? list? a)) (AppC n (parse a))]
    [else (error 'parse "invalid input to parse")]))

(check-equal? (parse '(+ 1 2))
              (PlusC (NumC 1) (NumC 2)))
(check-equal? (parse '(+ 1 var))
              (PlusC (NumC 1) (IdC 'var)))
(check-equal? (parse '(* (- 1) 2))
              (MultC (MultC (NumC -1) (NumC 1)) (NumC 2)))
(check-equal? (parse '(- 1 (+ 5 6)))
              (PlusC (NumC 1) (MultC (NumC -1) (PlusC (NumC 5) (NumC 6)))))
(check-equal? (parse '(func (+ 1 5)))
              (AppC 'func (PlusC (NumC 1) (NumC 5))))
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
; substitute

;;substitutes an expression for a symbol in another expression
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(AppC f a) (AppC f (subst what for a))]
    [(PlusC l r) (PlusC (subst what for l)
                        (subst what for r))]
    [(MultC l r) (MultC (subst what for l)
                        (subst what for r))]))

(check-equal? (subst (NumC 4) 'x (PlusC (IdC 'x) (NumC 1)))
              (PlusC (NumC 4) (NumC 1)))
(check-equal? (subst (NumC 4) 'x (AppC 'func (MultC (IdC 'x) (IdC 'y))))
              (AppC 'func (MultC (NumC 4) (IdC 'y))))

;-------------------------------------------------------------------------------------------
; get-fundef

;;gets a function definition from a list of FunDefC's, given its name
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
    (cond
      [(empty? fds)
       (error 'get-fundef "reference to undefined function")]
      [(cons? fds)
       (cond
         [(equal? n (FunDefC-name (first fds))) (first fds)]
         [else (get-fundef n (rest fds))])]))

(check-equal? (get-fundef 'func1 (list (FunDefC 'func2 'x (NumC 4))
                                       (FunDefC 'func1 'x (NumC 5))))
              (FunDefC 'func1 'x (NumC 5)))
(check-exn (regexp (regexp-quote "get-fundef: reference to undefined function"))
           (lambda () (get-fundef 'func '())))

;-------------------------------------------------------------------------------------------
; interp

;;evaluates an ExprC to a value
(define (interp [a : ExprC] [fds : (Listof FunDefC)]) : Real
    (match a
      [(NumC n) n]
      [(PlusC l r) (+ (interp l fds) (interp r fds))]
      [(MultC  l r) (* (interp l fds) (interp r fds))]
      [(AppC f a) (define fd (get-fundef f fds))
                  (interp (subst a (FunDefC-arg fd) (FunDefC-body fd)) fds)]))

(check-= (interp (PlusC (NumC 4) (NumC 5)) '()) 9 EPSILON)
(check-= (interp (MultC (NumC 4) (NumC 5)) '()) 20 EPSILON)
(check-= (interp (PlusC (MultC (NumC 3) (NumC 2)) (NumC 5)) '()) 11 EPSILON)
(check-= (interp (AppC 'func (NumC 3))
                 (list (FunDefC 'func 'x (PlusC (IdC 'x) (NumC 1)))))
          4 EPSILON)


;-------------------------------------------------------------------------------------------
; top-interp

;;interperates an S-expression
(define (top-interp [s : Sexp]) : Real
  (interp (parse s) '()))

(check-equal? (top-interp '{+ 2 3}) 5)
(check-equal? (top-interp '{* 2 {- {- {- 5 2}}}}) 6)
(check-equal? (top-interp '{+ {* 4 {- 2}} {- 10 5}}) -3)










#lang typed/racket

(require typed/rackunit)

(define EPSILON 1e-05)

;-------------------------------------------------------------------------------------------
; ArithC

(define-type ArithC (U NumC PlusC MultC))
(struct NumC ([n : Real]) #:transparent)
(struct PlusC ([l : ArithC] [r : ArithC]) #:transparent)
(struct MultC ([l : ArithC] [r : ArithC]) #:transparent)

(define-type ArithS (U NumS PlusS MinusS MultS UnaryS))
  (struct NumS ([n : Real]) #:transparent)
  (struct PlusS ([l : ArithS] [r : ArithS]) #:transparent)
  (struct MinusS ([l : ArithS] [r : ArithS]) #:transparent)
  (struct MultS ([l : ArithS] [r : ArithS]) #:transparent)
  (struct UnaryS ([e : ArithS]) #:transparent)

;;parses an S-exp and produces a corresponding ArithS
(define (parse1 [s : Sexp]) : ArithS
  (match s
    [(? real? a) (NumS s)]
    [(list '+ a b) (PlusS (parse1 a) (parse1 b))]
    [(list '* a b) (MultS (parse1 a) (parse1 b))]
    [(list '- a b) (MinusS (parse1 a) (parse1 b))]
    [(list '- a) (UnaryS (parse1 a))]
    [else (error 'parse1 "invalid input")]))

(check-equal? (parse1 '(+ 1 2)) (PlusS (NumS 1) (NumS 2)))
(check-equal? (parse1 '(+ 1 (- 2))) (PlusS (NumS 1) (UnaryS (NumS 2))))
(check-equal? (parse1 '(+ (* 3 6) (- 10 2))) (PlusS (MultS (NumS 3) (NumS 6))
                                                    (MinusS (NumS 10) (NumS 2))))
(check-exn (regexp (regexp-quote "invalid input"))
           (lambda () (parse1 "hello")))


;;desugars and ArithS into an ArithC
(define (desugar [as : ArithS]) : ArithC
    (match as
      [(NumS n) (NumC n)]
      [(PlusS l r) (PlusC (desugar l) (desugar r))]
      [(MultS l r) (MultC (desugar l) (desugar r))]
      [(UnaryS e) (MultC (NumC -1) (desugar e))]
      [(MinusS l r) (PlusC (desugar l)(MultC (NumC -1)(desugar r)))]))

(check-equal? (desugar (PlusS (NumS 1) (NumS 2)))
              (PlusC (NumC 1) (NumC 2)))
(check-equal? (desugar (MultS (UnaryS (NumS 1)) (NumS 2)))
              (MultC (MultC (NumC -1) (NumC 1)) (NumC 2)))
(check-equal? (desugar (MinusS (NumS 1) (PlusS (NumS 5) (NumS 6))))
              (PlusC (NumC 1) (MultC (NumC -1) (PlusC (NumC 5) (NumC 6)))))

;-------------------------------------------------------------------------------------------
; interp

;;evaluates an ArithC to a value
(define (interp [a : ArithC]) : Real
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
  (interp (desugar (parse1 s))))

(check-equal? (top-interp '(+ 2 3)) 5)
(check-equal? (top-interp '(* 2 (- (- (- 5 2))))) 6)
(check-equal? (top-interp '(+ (* 4 (- 2)) (- 10 5))) -3)










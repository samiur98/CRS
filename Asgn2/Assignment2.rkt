#lang typed/racket

(require typed/rackunit)

(define EPSILON 1e-05)

;Division BY ZERO AND DXUQ ERROR CHECK IS COMPLETE
;-------------------------------------------------------------------------------------------
; Data Definitions:
;   ExprC, FunDefC

(define-type ExprC (U NumC IdC AppC BinOp Ifleq0))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct AppC ([fun : Symbol] [args : (Listof ExprC)]) #:transparent)
(struct BinOp ([l : ExprC] [r : ExprC] [operator : Symbol]) #:transparent)
(struct Ifleq0 ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)

;BinOp new struct which replaces the PlusC and MultC

(struct FunDefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)

;-------------------------------------------------------------------------------------------
; Parse
;Parse mod is the new parse
;

(define (parse-mod [s : Sexp]) : ExprC
  (match s
    [(? real? a) (NumC a)]
    [(? symbol? id) (IdC id)]
    [(list '+ a b) (BinOp (parse-mod a) (parse-mod b) '+)]
    [(list '- a b) (BinOp (parse-mod a) (BinOp (parse-mod -1) (parse-mod b) '*) '+)]
    [(list '* a b) (BinOp (parse-mod a) (parse-mod b) '*)]
    [(list '/ a b) (BinOp (parse-mod a) (parse-mod b) '/)]
    [(list '- a) (BinOp (parse-mod -1) (parse-mod a) '*)]
    [(list 'ifleq0 test then else) (Ifleq0 (parse-mod test) (parse-mod then) (parse-mod else))]
   
    [(list (? symbol? n) args ...) (AppC n (map parse-mod (cast args (Listof Sexp))))]
    [else (error 'parse-mod "DXUQ invalid input to parse")]))

(check-equal? (parse-mod '{+ 1 2}) (BinOp (NumC 1) (NumC 2) '+))
(check-equal? (parse-mod '{* 1 2}) (BinOp (NumC 1) (NumC 2) '*))
(check-equal? (parse-mod '{- 5 2}) (BinOp (NumC 5) (BinOp (NumC -1) (NumC 2) '*) '+))
(check-equal? (parse-mod '{- 9}) (BinOp (NumC -1) (NumC 9) '*))
(check-equal? (parse-mod '{/ 10 2}) (BinOp (NumC 10) (NumC 2) '/))
(check-equal? (parse-mod '{/ 8 0}) (BinOp (NumC 8) (NumC 0) '/))
(check-equal? (parse-mod '{ifleq0 0 1 2}) (Ifleq0 (NumC 0) (NumC 1) (NumC 2)))
(check-equal? (parse-mod '{ifleq0 8 8 8}) (Ifleq0 (NumC 8) (NumC 8) (NumC 8)))



;;maps an s-expression directly to an ExprC
(check-equal? (parse-mod '{+ 1 2})
              (BinOp (NumC 1) (NumC 2) '+))
(check-equal? (parse-mod '{+ 1 var})
              (BinOp (NumC 1) (IdC 'var) '+))
(check-equal? (parse-mod '{* {- 1} 2})
              (BinOp (BinOp (NumC -1) (NumC 1) '*) (NumC 2) '*))
(check-equal? (parse-mod '{- 1 {+ 5 6}})
              (BinOp (NumC 1) (BinOp (NumC -1) (BinOp (NumC 5) (NumC 6) '+) '*) '+))
(check-equal? (parse-mod '{func {+ 1 5}})
              (AppC 'func (list(BinOp (NumC 1) (NumC 5) '+))))
(check-equal? (parse-mod '{ifleq0 1 {+ 2 2} {- 5 2}})
              (Ifleq0 (NumC 1) (BinOp (NumC 2) (NumC 2) '+) (BinOp (NumC 5)
                 (BinOp (NumC -1) (NumC 2) '*) '+)))
(check-equal? (parse-mod '{func})
              (AppC 'func '()))
(check-exn (regexp (regexp-quote "DXUQ invalid input to parse"))
           (lambda () (parse-mod "hello")))

;;parses a function definition, from an s-expression, into a FunDefC
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list 'fn (list (? symbol? name) (? symbol? args) ...) body)
     (FunDefC name (cast args (Listof Symbol)) (parse-mod body))]
    [else (error 'parse-fundef "DXUQ invalid input to parse-fundef")]))

(check-equal? (parse-fundef '{fn {myfunc x y z} {+ 1 x}})
              (FunDefC 'myfunc '(x y z) (BinOp (NumC 1) (IdC 'x) '+)))
(check-equal? (parse-fundef '{fn {myfunc} 2})
              (FunDefC 'myfunc '() (NumC 2)))
(check-exn (regexp (regexp-quote "DXUQ invalid input to parse-fundef"))
           (lambda () (parse-fundef '(+ 1 4))))

;;parses a list a functions, given as an s-expression
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
    [(list funs ...) (map parse-fundef funs)]))

(check-equal? (parse-prog '{})'())
(check-equal? (parse-prog '{{fn {myfunc x y z} {+ 1 x}}})
              (list (FunDefC 'myfunc '(x y z) (BinOp (NumC 1) (IdC 'x) '+))))
(check-equal? (parse-prog '{{fn {myfunc1 x y z} {+ 1 x}}
                            {fn {myfunc2} 1}})
              (list (FunDefC 'myfunc1 '(x y z) (BinOp (NumC 1) (IdC 'x) '+))
                    (FunDefC 'myfunc2 '() (NumC 1))))

; substitute

;;substitutes an expression for a symbol in another expression
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
                        (subst what for r) '*)]))


(check-equal? (subst (NumC 4) 'x (BinOp (IdC 'x) (NumC 1) '+))
              (BinOp (NumC 4) (NumC 1) '+))

(check-equal? (subst (NumC 4) 'x (AppC 'func (list (IdC 'x) (BinOp (IdC 'x) (IdC 'y) '*))))
              (AppC 'func (list (NumC 4) (BinOp (NumC 4) (IdC 'y) '*))))


;;calls subst for each argument in 'what', handling multi-arg functions
(define (subst-args [args : (Listof ExprC)] [params : (Listof Symbol)] [in : ExprC]) : ExprC
  (cond
    [(empty? args) in]
    [else (subst-args (rest args) (rest params) (subst (first args) (first params) in))]))

(check-equal? (subst-args (list (NumC 4) (NumC 8)) '(x y) (BinOp (IdC 'x) (IdC 'y) '+))
              (BinOp (NumC 4) (NumC 8) '+))


;;gets a function definition from a list of FunDefC's, given its name
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
    (cond
      [(empty? fds)
       (error 'get-fundef "DXUQ reference to undefined function")]
      [(cons? fds)
       (cond
         [(equal? n (FunDefC-name (first fds))) (first fds)]
         [else (get-fundef n (rest fds))])]))

(check-equal? (get-fundef 'func1 (list (FunDefC 'func2 '(x) (NumC 4))
                                       (FunDefC 'func1 '(x) (NumC 5))))
              (FunDefC 'func1 '(x) (NumC 5)))
(check-exn (regexp (regexp-quote "get-fundef: DXUQ reference to undefined function"))
           (lambda () (get-fundef 'func '())))


; interp
;;evaluates an ExprC to a value
(define (interp [a : ExprC] [fds : (Listof FunDefC)]) : Real
    (match a
      [(NumC n) n]
      [(BinOp l r '+) (+ (interp l fds) (interp r fds))]
      [(BinOp  l r '*) (* (interp l fds) (interp r fds))]
      [(BinOp l r '/) (/ (interp l fds) (match (interp r fds)
                                          [0 (error "DXUQ Cannot divide by Zero")]
                                          [(? real? r) r]))]
      [(Ifleq0 test then e)
         (cond
           [(<= (interp test fds) 0) (interp then fds)]
            [else (interp e fds)])]
      [(AppC f args) (define fd (get-fundef f fds))
                  (interp (subst-args args (FunDefC-args fd) (FunDefC-body fd)) fds)]))


(check-= (interp (BinOp (NumC 10) (NumC 2) '/) '()) 5 EPSILON)
(check-exn (regexp (regexp-quote "DXUQ Cannot divide by Zero"))
           (lambda () (interp (BinOp (NumC 10) (NumC 0) '/) '())))

(check-= (interp (BinOp (NumC 4) (NumC 5) '+) '()) 9 EPSILON)
(check-= (interp (BinOp (NumC 4) (NumC 5) '*) '()) 20 EPSILON)
(check-= (interp (BinOp (BinOp (NumC 3) (NumC 2) '*) (NumC 5) '+) '()) 11 EPSILON)
(check-= (interp (AppC 'func (list (NumC 10) (NumC 5)))
                 (list (FunDefC 'func '(x y) (BinOp (IdC 'x) (IdC 'y) '+))))
          15 EPSILON)
(check-= (interp (Ifleq0 (NumC 0) (NumC 1) (NumC 2)) '()) 1 EPSILON)
(check-= (interp (Ifleq0 (NumC -1) (NumC 1) (NumC 2)) '()) 1 EPSILON)
(check-= (interp (Ifleq0 (NumC 1) (NumC 1) (NumC 2)) '()) 2 EPSILON)


;;intreprets the 'main' function, given a list of functions
(define (interp-fns [funs : (Listof FunDefC)]) : Real
  (interp (FunDefC-body (get-fundef 'main funs)) funs))

(check-equal? (interp-fns
               (list (FunDefC 'func '(x) (NumC 4)) (FunDefC 'main '(x) (NumC 5))))5)
(check-equal? (interp-fns
       (parse-prog '{{fn {f x y} {+ x y}}
                     {fn {main} {f 1 2}}})) 3)
(check-equal? (interp-fns
        (parse-prog '{{fn {f} 5}
                      {fn {main} {+ {f} {f}}}}))10)


;;interperates an S-expression
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))


(check-equal? (top-interp '{{fn {f x y} {+ x y}}
                            {fn {main} {f 1 2}}}) 3)
(check-equal? (top-interp '{{fn {f} 5}
                            {fn {main} {+ {f} {f}}}})10)
(check-equal? (top-interp '{{fn {u x} {- x}}
                            {fn {f x y} {* x y}}
                            {fn {main} {+ {f 6 4} {u 4}}}})20)

(check-equal? (top-interp '{{fn {u x} {- x}}
                            {fn {f x y} {* x y}}
                            {fn {main} {+ {f 6 4} {u {- 6}}}}})30)
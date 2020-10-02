# CRS
Repo for CSC 430

Note:
In the lab, our parser would parse code that looked like this: '(+ 4 (- 10 5))
Now, our parser will parse code that looks like this: '{+ 4 {- 10 5}}

BUT, Racket treats these exactly the same, so '(+ 4 (- 10 5)) equals '{+ 4 {- 10 5}} in racket


TODO Tasks:
 1. (Rohan) Convert to using a single "BinaryOperation" struct instead of separate Plus, Mult, etc. structs
 	Do this in ExprC struct
 	Modify parse to create binop structs instead of plus, mult, etc.
 	Modify interpreter to evaluate binop structs 
	~ I am done with this ~ Rohan

 2. Add an "if statement", called ifleq0, to the ExprC struct, and 
 	modify the parser to accept ifleq0 s-expression and return ifleq0 structs, and 
 	modify the interpreter to evaluate ifleq0 structs

 3. (I'll start with this part - Connor)Functions
 	create required data structures for functions
 	modify parser to read in funcdef s-expressions and return fundefC struct
 	modify intpreter to evaluate fundefC structs

 4. Divison (also prevent divide by zero)

 5. Correct Errors - thrown at the right times and with the right messages


 Functions that are done (some may need to be edited as we make more changes)
 - (parse s) → ExprC
 - (parse-fundef s) → FundefC
 - (parse-prog s) → (listof FundefC)
 - (interp-fns funs) → Real
 - (interp exp funs) → Real
 - (top-interp fun-sexps) → Real

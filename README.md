# CRS
Repo for CSC 430

Note:
In the lab, our parser would parse code that looked like this: '(+ 4 (- 10 5))
Now, our parser will parse code that looks like this: '{+ 4 {- 10 5}}

BUT, Racket treats these exactly the same, so '(+ 4 (- 10 5)) equals '{+ 4 {- 10 5}} in racket


TODO Tasks:
 1. Convert to using a single "BinaryOperation" struct instead of separate Plus, Mult, etc. structs
 	Do this in both ArithC and ArithS
 	Modify parse to create binop structs instead of plus, mult, etc.
 	Modify desugar to work with binop structs instead of plus, mult, etc.
 	Modify interpreter to evaluate binop structs
 2. Add an "if statement", called ifleq0, to the ExprC struct, and 
 	modify the parser to accept ifleq0 s-expression and return ifleq0 structs, and 
 	modify the interpreter to evaluate ifleq0 structs
 3. Functions
 	create required data structures for functions
 	modify parser to read in funcdef s-expressions and return fundefC struct
 	modify intpreter to evaluate fundefC structs

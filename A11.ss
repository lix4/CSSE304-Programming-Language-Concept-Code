;Xiwen Li
;Assignment11

(load "chez-init.ss")

(define-datatype expression expression?
	[var-exp (id symbol?)]
	[lambda-list-vars-exp 
		(id (list-checker symbol?))
		(body (list-checker expression?))
	]
	[lambda-improp-exp
		(id (list-checker improperlist?))
		(body (list-checker expression?))
	]
	[lambda-sym-exp
		(id symbol?)
		(body (list-checker expression?))
	]
	[app-exp
		(rator expression?)
		(rand (list-checker expression?))
		]
	[lit-exp (id any-type?)]
	[if-exp 
		(condition expression?)
		(true expression?)
		(false expression?)
	]
	[if-no-else-exp
		(condition expression?)
		(true expression?)
	]
	[letrec-exp 
		(id list?)
		(body (list-checker expression?))
	]
	[let-exp 
		(id list?)
		(body (list-checker expression?))
	]
	[let*-exp 
		(id list?)
		(body (list-checker expression?))
	]
	[set!-exp 
		(var symbol?)
		(val expression?)
	]
	)

(define list-checker
	(lambda (pred)
		(lambda (ls)
			(or (null? ls) (and (pair? ls) (pred (car ls)) ((list-checker pred) (cdr ls)) ))
			)
		)
	)

(define improperlist?
  (lambda (x)
    (and (pair? x) (not (list? x)))))

(define any-type?
	(lambda (val)
		(or (symbol? val) (number? val) (vector? val) (boolean? val))
		)
	)

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)


(define parse-exp         
  	(lambda (datum)
    	(cond
		    [(symbol? datum) (var-exp datum)]
		    [(literal? datum) (lit-exp datum)]
		    [(not (list? datum)) (eopl:error 'parse-exp "application ~s is not a proper list" datum)]
		    [(pair? datum)
      			(cond
      				[(eqv? (1st datum) 'lambda) 
      				(cond 
      					[(< (length datum) 3) (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)]
      					[else 
      					(cond
      						[(list? (2nd datum)) 
      						(if (not (andmap symbol? (2nd datum))) 
      							(eopl:error 'parse-exp "lambda argument list: formals must be symbols: ~s" (2nd datum))
      							(lambda-list-vars-exp (2nd datum) (map parse-exp (cddr datum)))
      						)]
      						[(symbol? (2nd datum)) (lambda-sym-exp (2nd datum) (map parse-exp (cddr datum)))]
      						[(improperlist (2nd datum)) (lambda-improp-exp (2nd datum)) (map parse-exp (cddr datum))]
							)
      					]
      					)]
      				[(eqv? (1st datum) 'if) 
      				(cond 
      					[(> (length datum) 4) (eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" datum)]
      					[(<= (length datum) 2) (eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" datum)]
      					[(= (length datum) 3) (if-no-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
      					[else (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (cadddr datum)))]
      					)]
      				[(eqv? (1st datum) 'let) 
      				(cond
      					[(<= (length datum) 2) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" datum)]
      					[(improperlist? (2nd datum)) (eopl:error 'parse-exp "Error in parse-exp decls: not a proper list: ~s" (2nd datum))]
      					[(and (not (eqv? (2nd datum) '())) (ormap improperlist? (2nd datum))) (eopl:error 'parse-exp "Error in parse-exp: decls: not all proper lists: ~s" (2nd datum))]
      					[(not (andmap (lambda (ls) (eqv? (length ls) 2)) (2nd datum))) (eopl:error 'parse-exp "decls: not all length 2: ~s" (2nd datum))]
      					[(not (andmap symbol? (map car (2nd datum)))) (eopl:error 'parse-exp "decls: first members must be symbols: ~s" (2nd datum))]
      					
      					[else (let-exp (map l-id-process (2nd datum)) (map parse-exp (cddr datum)))]
      					)]
      				[(eqv? (1st datum) 'let*)
      				(cond
      					[(not (list? (2nd datum))) (eopl:error 'parse-exp "Error in parse-exp: let* declarations not a list" datum)]
      					[(<= (length datum) 2) (eopl:error 'parse-exp "~s-expression has incorrect length ~s" datum)]
      					[else 
      					(cond
      						[(not (andmap (lambda (ls) (eqv? (length ls) 2)) (2nd datum))) (eopl:error 'parse-exp "decls: not all length 2: ~s" (2nd datum))]
      						[(not (andmap symbol? (map car (2nd datum)))) (eopl:error 'parse-exp "decls: first members must be symbols: ~s" (2nd datum))]
      						[else (let*-exp (map l-id-process (2nd datum)) (map parse-exp (cddr datum)))]
      						)
      					]
      					)
      				]
      				[(eqv? (1st datum) 'letrec)
      				(cond
      					[(<= (length datum) 2) (eopl:error 'parse-exp "Error in parse-expression: letrec expression: incorrect length: ~s" datum)]
      					[(not (list? (2nd datum))) (eopl:error 'parse-exp "Error in parse-exp: letrec: declarations is not a list" datum)]
      					[(not (andmap (lambda (ls) (eqv? (length ls) 2)) (2nd datum))) (eopl:error 'parse-exp "decls: not all length 2: ~s" (2nd datum))]
      					[(not (andmap symbol? (map car (2nd datum)))) (eopl:error 'parse-exp "decls: first members must be symbols: ~s" (2nd datum))]
      					[else (letrec-exp (map l-id-process (2nd datum)) (map parse-exp (cddr datum)))]			
      					)
      				]
      				[(eqv? (1st datum) 'set!) 
      				(cond
      					[(= (length datum) 3) (set!-exp (2nd datum) (parse-exp (3rd datum)))]
      					[(<= (length datum) 2) (eopl:error 'parse-exp "set! expression ~s does not have (only) variable and expression" datum)]
      					[(> (length datum) 3) (eopl:error 'parse-exp "set! expression ~s is too long" datum)]
      					)]
       				[else (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))]
       				)
      			]
     		[else (eopl:error 'parse-exp "bad expression: ~s" datum)]
     	)
    )
)

(define unparse-exp
	(lambda (exp)
		(cases expression exp
			[var-exp (datum) datum]
			[lambda-sym-exp (id body) (append (list 'lambda id) (map unparse-exp body))]
			[lambda-list-vars-exp (id body) (append (list 'lambda id) (map unparse-exp body))]
			[lambda-improp-exp (id body) (list 'lambda id (map unparse-exp body))]
			[app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))]
			[lit-exp (id) id]
			[if-exp (condition true false) (list 'if (unparse-exp condition) (unparse-exp true) (unparse-exp false))]
			[if-no-else-exp (condition true) (list 'if (unparse-exp condition) (unparse-exp true))]
			[letrec-exp (id body) (append (list 'letrec (map l-id-process-for-unparse id)) (map unparse-exp body))]
			[let-exp (id body) (append (list 'let (map l-id-process-for-unparse id)) (map unparse-exp body))]
			[let*-exp (id body) (append (list 'let* (map l-id-process-for-unparse id)) (map unparse-exp body))]
			[set!-exp (var val) (list var (unparse-exp val)) ]
			)
		)
	)

(define l-id-process-for-unparse
	(lambda (x) 
		(list (1st x) (unparse-exp (2nd x)))
		)
	)

(define l-id-process
	(lambda (x) 
		(list (1st x) (parse-exp (2nd x)))
		)
	)

(define literal?
	(lambda (val)
	(or (number? val) (boolean? val) (symbol? val) (vector? val) (string? val))
		)
	)


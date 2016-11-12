;;Xiwen Li
;;Assignment 11b

(load "chez-init.ss"); This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define-datatype expression expression?
	[var-exp (id symbol?)]
	[lit-exp (id literals?)]
	[lambda-exp (id symbol?)
	            (body expression?)]
	[app-exp (rator expression?)
	         (rand expression?)]
	[if-exp (test expression?)
	        (true expression?)
	        (false expression?)]
	[if-no-else-exp (test expression?)
	                (true expression?)]
	[let-exp (id list?)
	         (body (list-of expression?))]
	[let*-exp (id list?)
	          (body (list-of expression?))]
	[letrec-exp (id list?)
	            (body (list-of expression?))]
	[set!-exp (var symbol?)
	          (val expression?)])

(define literals? 
	(lambda (obj)
		(or (number? obj) 
			(symbol? obj)
			(string? obj)
			; (list? obj)
			(boolean? obj))))

(define parse-exp         
	(lambda (datum)
		(cond 
			  [(symbol? datum) (var-exp datum)]
			  [(literals? datum) (lit-exp datum)]
			  [(pair? datum)
			   (cond [(eqv? (car datum) 'lambda)
				      (lambda-exp (car (2nd datum))
					  (parse-exp (3rd datum)))]
					 [(eqv? (1st datum) 'if)
					  (if (null? (cddr datum))
					   	  ())]
					 [(eqv? (1st datum) 'let)
					  (let-exp (2nd datum)
					  	       (parse-exp (3rd datum)))]
					 [(eqv? (1st datum) 'let*)
					  (let*-exp (2nd datum)
					  	       (parse-exp (3rd datum)))]
					 [(eqv? (1st datum) 'letrec)
					  (letrec (2nd datum) (parse-exp (3nd datum)))]
					 [(eqv? (1st datum) 'set!)
					  (set!-exp (2nd datum) (parse-exp (3rd datum)))]
				     [else (app-exp (parse-exp (1st datum)) 
					                (parse-exp (2nd datum)))])]
			  [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))



; Procedures to make the parser a little bit saner.

(define parse-exp         
  (lambda (datum)
	(begin
		(cond
			[(symbol? datum) (var-exp datum)]
			[(value? datum) (lit-exp datum)]
			[(vector? datum) (vec-exp datum)]
			[(pair? datum)
				(cond
					[(not(list? datum))
						(lit-exp datum)]	
					[(syntax? (car datum)) 
						(parse-exp (syntax-expand datum))]						
					[(eq? (car datum) 'lambda)
						(lambda-do datum)]
					[(eq? 'define (car datum))
              (def-do datum)]
					[(eqv? (car datum) 'if)
						(if-do (cdr datum))]
					[(eqv? (car datum) 'quote)
						(parse-do datum)]
					[(eqv? (car datum) 'set!)
						(set!-do datum)]
					[else (app-exp (parse-exp (car datum))
				  (map parse-exp (cdr datum)))])]
			[else (eopl:error 'parse-exp "bad expression: ~s" datum)]))))
			
			
(define if-do
	(lambda(x)
		(cond
			[(> (length x) 3)
				(eopl:error 'parse-exp "if expression: too many clause ~s" x)]
			[(< (length x) 2)
				(eopl:error 'parse-exp "if expression: missing clause ~s" x)]
			[(= (length x) 2)
				(if-exp (parse-exp (car x))(parse-exp (cadr x)))]
			[else
				(else-exp (parse-exp (car x))(parse-exp (cadr  x))(parse-exp (caddr x)))])))
				
				
(define lambda-do
  (lambda (x)
    (cond ((< (length x) 3)
	     (eopl:error 'parse-exp
                         "lambda expression: illegal expression ~s"
                         x))
          (else (lambda-exp (cadr x) 
                            (map parse-exp (cddr x)))))))
			
(define parse-do
	(lambda(x)
		(quote-exp (cadr x))))
		

(define set!-do
  (lambda (x)
    (let ((para (length (cddr x))))
         (cond ((< para 1)
                (eopl:error 'parse-exp
                            "set! expression: illegal length ~s"
                            x))
               (else (set!-exp (cadr x) 
                               (parse-exp (caddr x))))))))
			
	(define def-do
		(lambda (x)
			(begin
			(def-exp (cadr x) (parse-exp (caddr x))))))





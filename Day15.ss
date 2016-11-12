(define-syntax my-let
	(syntax-rules ()
		[(_ ([var1 val1] ...) e1 e2 ...) 
		((lambda (var1 ...) e1 e2 ...) val1 ...)]))

(define-syntax my-if
	(syntax-rules (then else)
		[(_ e1 then e2) (if e1 e2)]
		[(_ e1 then e2 else e3) (if e1 e2 e3)]))

(define-syntax ++
	(syntax-rules ()
		[(_ x) (begin (set! x (+ x 1)) x)]))

(define-syntax ++post
	(syntax-rules ()
		[(_ x)
		 (let ([temp x])
		 	(set! x (+ 1 x))
		 	temp)]))

(define-syntax my-and
	(syntax-rules ()
		[(_) #t]
		[(_ exp) exp]
		[(_ exp1 exp2 ...)
		 (if exp1
		 	 (my-and exp2 ...)
		 	 #f)]))

(define-syntax for
	(syntax-rules (:)
		[(_ ((init ...) : test : update ...) body ...) 
		 (begin 
		 	init ...
		 	(let loop ()
		 		(if test 
		 			(begin
		 				body ...
		 				update ...
		 				(loop)))))]))

(for (((define i 0) (define j 1)) : (< i 7) : (++ i) (set! j (* 2 j))) (display i) (display " ") (display j))
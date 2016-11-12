;;Xiwen Li
;;Assignment 10a

;;1
(define free-vars
	(lambda (exp)
		(free-vars-rec exp '())))

(define free-vars-rec
	(lambda (exp current)
		(cond [(null? exp) current]
			  [(symbol? exp) 
			   (if (member? exp current)
			   	   current
			   	   (append current (list exp)))]
			  [(equal? (car exp) 'lambda)
			   (cond [(symbol? (caddr exp)) (if (member? (caddr exp) (cadr exp))
			   									current
			   									(if (member? (caddr exp) current)
			   	   									current
			   	   							        (append current (list (caddr exp)))))]
			   	     [(equal? (caaddr exp) 'lambda) (free-vars-rec (caddr exp) current)]
			   	     [else (free-vars-check-rec (cadr exp) (caddr exp) current)])]
			  ; [else (if (and (symbol? (car exp)) (equal? (car exp) (cadr exp)))
			  [else (if (symbol? (car exp))
				  		(if (member? (car exp) current)
				   	   		(free-vars-rec (cadr exp) current)
				   	   		(free-vars-rec (cadr exp) (append current (list (car exp)))))
				  		(free-vars-rec (cadr exp) (free-vars-rec (car exp) current)))])))
			  			; (list (car exp))
			  			; (append (free-vars (car exp)) (free-vars (cadr exp))))]))

(define free-vars-check-rec
	(lambda (target ls current)
		(cond [(null? ls) current]
			  [(equal? (car ls) (car target)) (free-vars-check-rec target (cdr ls) current)]
			  [else (if (member? (car ls) current)
			  			(free-vars-check-rec target (cdr ls) current)
			  			(free-vars-check-rec target (cdr ls) (append current (list (car ls)))))])))

(define bound-vars
	(lambda (exp)
		(cond
			[(symbol? exp) '()]
			[(equal? (car exp) 'lambda)
				(cond
					[(symbol? (caddr exp)) 
						(if (equal? (caadr exp) (caddr exp)) 
							(append (list (caddr exp)))
							(append '())
					)
					]
					[(equal? (car (caddr exp)) 'lambda) 
						(if (equal? (caadr exp) (caddr (caddr exp)) )
							(append (list (caadr exp)))
							(append (bound-vars (caddr exp)))
							)
					]
					[(pair? (caddr exp)) (append (bound-vars (car (caddr exp))) (bound-vars (cadr (caddr exp))))]					)
			]
			[(pair? exp) (append (bound-vars (car exp)) (bound-vars (cadr exp)))]
			)
		)
	)

(define member?
	(lambda (a ls)
		(cond
			[(null? ls) #f]
			[(equal? (car ls) a) #t]
			[else (member? a (cdr ls))]
			)
		)
	)
;;2
(define let->application
	(lambda (ls)
		(append (list (append (append (list 'lambda) (list (map car (cadr ls)))) (cddr ls))) (map cadr (cadr ls)))
		)
	)

(define let*->let
	(lambda (ls)
		 (let*->let-rec (cadr ls) (caddr ls))
		)
	)

(define let*->let-rec
	(lambda (ls last)
		(cond
			[(null? ls) last]
			[else (append (list 'let) (list (list (car ls))) (list (let*->let-rec (cdr ls) last)) )]
			)
		)
	)

(define occurs-free?
	(lambda (var exp)
		(cond
			[(null? exp) #f]
			[(symbol? exp) (eqv? var exp)]
			[(eqv? (car exp) 'lambda) 
				(and 
					(not (member var (cadr exp))) 
					(occurs-free? var (caddr exp))
					)]
			[(equal? (car exp) 'let) (occurs-free? var (let->application exp))]
			[(equal? (car exp) 'let*) (occurs-free? var (let*->let exp))]
			[(equal? (car exp) 'if) 
			(if (member var (cadr exp))
				#t
				(occurs-free? var (caddr exp))
			)]
			[(equal? (car exp) 'set!) 
				(cond
					[(equal? var (caddr exp)) #t]
					[else #f]
					)]
			[else 
			(cond
				[(occurs-free? var (car exp)) #t]
				[else (occurs-free? var (cdr exp))]
				)]
			)
		)
	)



(define occurs-bound?
	(lambda (var exp)
		(cond
			[(null? exp) #f]
			[(symbol? exp) #f]
			[(eqv? (car exp) 'lambda) 
			(or 
				(occurs-bound? var (caddr exp)) 
				(and (member var (cadr exp)) (occurs-free? var (caddr exp))) 
				)]
			[(equal? (car exp) 'let) (occurs-bound? var (let->application exp))]
			[(equal? (car exp) 'let*) (occurs-bound? var (let*->let exp))]
			[(equal? (car exp) 'if) 
			(if (member var (cadr exp))
				#f
				(occurs-free? var (caddr exp))
			)]
			[(equal? (car exp) 'set!) 
			(cond
				[(or (equal? var (cadr exp)) (equal? var (car exp))) #f]
				)
			]
			[else 
			(cond
				[(occurs-bound? var (car exp)) #t]
				[else (occurs-bound? var (cdr exp))]
				)]
			)
		)
	)

;;3
(define lexical-address
	(lambda (exp)
		(lexical-address-rec exp '())))

(define lexical-address-rec 
	(lambda (exp current depth)
		(cond [(null? exp) '()]
			  [(symbol? exp) (list ':  )]
			  [(equal? (car exp) 'if) 
			   (list 'if 
			  		 (lexical-address-rec (cadr exp) (+ depth 1)) 
			  		 (lexical-address-rec (caddr exp) (+ depth 1)))]
			  [(equal? (car exp) 'lambda)
			   (cadr exp)]
			  [else (cons () ())])))

(define lexical-address-find
	(lambda (tar current)
		(cond [(null? current) (list ': 'free tar)]
			  [(equal? tar (car current)) (list ':  )]
			  [else (lexical-address-find tar (cdr current))])))


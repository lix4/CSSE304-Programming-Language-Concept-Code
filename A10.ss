;;Xiwen Li
;;Assignment 10


;;1
(define free-vars
	(lambda (exp)
		(cond [(symbol? exp) (list exp)]
			  [(equal? (car exp) 'lambda)
			   (cond [(symbol? (caddr exp)) (if (member? (caddr exp) (cadr exp))
			   									'()
			   									(list (caddr exp)))]
			   	     [(equal? (caaddr exp) 'lambda) (free-vars (caddr exp))]
			   	     [else (free-vars-rec (caddr exp) (cadr exp))])]
			  [else (if (and (symbol? (car exp)) (equal? (car exp) (cadr exp)))
			  			(list (car exp))
			  			(append (free-vars (car exp)) (free-vars (cadr exp))))])))

(define free-vars-rec
	(lambda (ls target)
		(cond [(null? ls) '()]
			  [(equal? (car ls) target) (free-vars-rec (cdr ls) target)]
			  [else (append (list (car ls)) (free-vars-rec (cdr ls) target))])))

(define bound-vars
	(lambda (exp)
		(cond [(symbol? exp) exp]
			  [(equal? (car exp) 'lambda) 
			   (let ([dels (cadr exp)] [body (caddr exp)])
			   		(cond [(symbol? body) (if (member? body dels)
			   								  (list body)
			   								  '())]
				   	      [(equal? (car body) 'lambda) (bound-vars body)]
				   	      [(equal? (caadr body) 'lambda) (bound-vars (cadr body))]
				   	      [else (bound-vars-rec body dels)]))]
			  [else (if (symbol? (car exp))
			  			(bound-vars (cadr exp))
			  			(append (bound-vars (car exp)) (bound-vars (cadr exp))))])))

(define bound-vars-rec
	(lambda (ls target)
		(cond [(null? ls) '()]
			  [(equal? (car ls) target) (append (list (car ls)) (bound-vars-rec (cdr ls) target))]
			  [else (bound-vars-rec (cdr ls) target)])))

(define member?
	(lambda (target ls)
		(cond [(null? ls) #f]
			  [(equal? (car ls) target) #t]
			  [else (member? target (cdr ls))])))

;;2
(define occurs-free?
  	(lambda (var exp)
    	(cond [(symbol? exp) (eqv? var exp)]
      		  [(eqv? (car exp) 'lambda) 
       		   (and (not (eqv? (caadr exp) var))
                    (occurs-free? var (caddr exp)))]
      		  [else (or (occurs-free? var  (car exp))
                		(occurs-free? var (cadr exp)))])))

(define occurs-bound?
  	(lambda (var exp)
    	(cond [(symbol? exp) #f]
      		  [(eqv? (car exp) 'lambda)
               (or (occurs-bound? var (caddr exp))
                   (and (eqv? (caadr exp) var)
                        (occurs-free? var (caddr exp))))]
      		  [else (or (occurs-bound? var  (car exp))
                        (occurs-bound? var (cadr exp)))])))

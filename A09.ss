;;Xiwen Li
;;Assignment 09	

;;1
;;slist-recur
(define slist-recur
	(lambda (base-value slist-proc slist-proc-slist)
		(letrec ([helper 
			     (lambda (slist)
			     	     (cond [(null? slist) base-value]
			     	     	   [(or (symbol? (car slist))
			     	     	        (number? (car slist))) 
			     	     	    (slist-proc (car slist) 
			     	     	   	            (helper (cdr slist)))]
			     	     	   [else (slist-proc-slist (helper (car slist)) 
			     	     	   	                       (helper (cdr slist)))]))])
				helper)))


;;(a)
(define sn-list-sum (slist-recur 0 + +))

;;(b)
(define sn-list-map 
	(lambda (proc slist)
		((slist-recur '()
			          (lambda (x y) (cons (proc x) y))
			          (lambda (x y) (cons x y))) slist)))

;;(c)
(define sn-list-paren-count 
	(slist-recur  2 
		          (lambda (x y) y) 
		          (lambda (x y) (+ x y))))

;;(d)
(define sn-list-reverse
	(slist-recur '()
		          (lambda (x y) (append y (list x)))
		          (lambda (x y) (append y (list x)))))

;;(e)
(define sn-list-occur
	(lambda (s snlst)
		((slist-recur 0
			         (lambda (x y)
			         	(if (equal? x s)
			         		(+ 1 y)
			         		y))
			         (lambda (x y)
			         	(+ x y))) snlst)))

;;(f)
(define sn-list-depth
	(lambda (snlist)
		(+ 1 ((slist-recur 0
			         (lambda (x y) y)
			         (lambda (x y) (max (+ x 1) y))) snlist))))
			         	
;;2
(define bt-recur
	(lambda (base-value root-proc number-proc)
		(letrec ([helper 
			      (lambda (bt)
			      	(cond [(null? bt) base-value]
			      		  [(number? bt) (number-proc bt)]
			      		  [else (root-proc (car bt)
			      		  	               (helper (cadr bt))
			      		  	               (helper(caddr bt)))]))])
			  	helper)))

(define bt-sum
	(bt-recur 0 
		      (lambda (x y z) (+ y z)) 
		      (lambda (x) x))) 

(define bt-inorder
	(bt-recur '() 
		       (lambda (x y z) (append y (list x) z)) 
		       (lambda (x) '())))

;;3
(define make-c...r
	(lambda (str)
		(let ([proc-list (map make-c...r-converter (string->list str))])		
			(lambda (ls)
				((apply compose proc-list) ls)))))

(define compose
	 (case-lambda
		 [() (lambda (x) x)]
		 [(first . rest)
			 (let ([composed-rest (apply compose rest)])
			 	  (lambda (x) (first (composed-rest x))))]))

(define make-c...r-converter
	(lambda (obj)
		(cond
			[(equal? #\a obj) car]
			[else cdr])))

;;4
(define make-slist-leaf-iterator
	 (lambda (slist)
		(let ([stack (make-stack)])
			(stack 'push slist)
			(letrec ([my-iterator 
					 (lambda ()
					  	 (if (stack 'empty?)
					  	 	 #f
					  	 	 (let ([temp (stack 'pop)])
					  	 	 	  (cond [(null? temp) (my-iterator)]
					  	 	 	  		[(symbol? temp) temp]
					  	 	 	  	    [else (inner-my-iterator temp)]))))]
					 [inner-my-iterator
					 (lambda (snlst)
					 	(cond [(null? snlst) (my-iterator)]
					 		  [(null? (car snlst)) (inner-my-iterator (cdr snlst))]
					 		  [(symbol? (car snlst)) (stack 'push (cdr snlst)) (car snlst)]
					 		  [else (stack 'push (cdr snlst)) (inner-my-iterator (car snlst))]))])
				my-iterator))))
			
			 	


(define make-stack
	(lambda ()
		 (let ([stk '()])
			 (lambda (msg . args)
				 (case msg ; Scheme's case is a similar to switch in some other languages.
					 [(empty?) (null? stk)]
					 [(push) (set! stk (cons (car args) stk))]
					 [(pop) (let ([top (car stk)])
					             (set! stk (cdr stk))
					             top)]
					 [else (errorf 'stack "illegal message to stack object: ~a" msg)])))))



			     	     	   



(define product-cps
 	(lambda (x y k)
 		(if (null? y)
			(apply-continuation k'())
			(let loop ([x x] [accum '()] [k k])
			 		(if (null? x)
			 			(apply-continuation k accum)
			 			(map-cps (lambda (s) (list (car x) s)) 
			 				      y
			 				     (lambda (v) 
			 				     	(append-cps v accum
			 				     		(lambda (x)
			 				     			(loop (cdr x)
			 				     				x
			 				     				k))))))))))
			 				     				

						; (loop (cdr x)
						;       (append (map (lambda (s) (list (car x) s)) y) accum)))))))

; (define expand-let
; 	(lambda (exp)
; 		(cases expression exp
; 			[lit-exp (id) (lit-exp id)]
; 			[if-exp (test-exp then-exp else-exp)
; 			     (if-exp test-exp then-exp else-exp)]
; 			[lambda-exp (id bodies) (lambda-exp id bodies)]
; 			[let-exp (vars vals bodies)
; 			        (app-exp (lambda-exp vars bodies) vals)]
; 			[app-exp ]


(define make-stack
	(lambda ()
 		(let ([stk '()])
 			(lambda (msg . args)
 				(case msg
				 [(empty?) (null? stk)]
				 [(push) (set! stk (cons (car args) stk))]
				 [(pop) (if (null? stk)
				            (errorf 'pop "attempt to pop empty stack")
							 (let ([top (car stk)])
							       (set! stk (cdr stk)) top))]
				 [(top) (car stk)]
				 [(display) (list '*stack* stk)]
				 [else (errorf 'stack "illegal message to stack object: ~a" msg)])))))


(define make-lyst
	(lambda ()
		(let ([stk (make-stack)])
			 (lambda (msg . arg)
			 	(case msg
			 		[(insert-current-position) (stk 'push val)]
			 		[(get-current-position) (stk 'top stk)]
			 		[(shift) (stk )]
			 		[()]
			 		; [(length) ]

(define apply-continuation
	(lambda (k v)
		(k v)))

(define subst-leftmost
	(lambda (new old slist pred)
		(cond [(null? slist) '()]
			  [(list? (car slist))
			   (let ([next-sbst (subst-leftmost new old (car slist) pred)])
			   	    (if (null? next-sbst)
			   	    	(cons (car slist) (subst-leftmost new old (cdr slist) pred))
			   	    	(cons next-sbst (subst-leftmost new old (cdr slist) pred))))]
			  [(pred old (car slist))
			   (cons new (cdr slist))]
			  [else (cons (car slist) (subst-leftmost new old (cdr slist) pred))])))

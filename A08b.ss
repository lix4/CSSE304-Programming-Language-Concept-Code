(define group-by-n
	(lambda (ls n)
		(group-by-n-rec ls n 0 '() '())))

(define group-by-n-rec
	(lambda (ls n index current current-element)
		(cond [(null? ls) '()]
			  [(null? (cdr ls)) (append current (list (append current-element (list (car ls)))))]
			  [(= index (- n 1))
			    (if (null? current)
			    	(group-by-n-rec (cdr ls) n 0 (list (append current-element (list (car ls)))) '())
			        (group-by-n-rec (cdr ls) n 0 (append current (list (append current-element (list (car ls))))) '()))]
			  [else (group-by-n-rec (cdr ls) n (+ index 1) current (append current-element (list (car ls))))])))

(define symmetric?
	(lambda (rel)
		(cond [(null? rel) #t]
			  [(equal? (caar rel)
			  	       (cadar rel))
			   (symmetric? (cdr rel))]
			  [else (member? (list (cadar rel) (caar rel)) rel)])))

(define member?
	(lambda (target ls)
		(cond [(null? ls) #f]
			  [(equal? (car ls) target) #t]
			  [else (member? target (cdr ls))])))



(define subst-leftmost
	(lambda (new old slist equality-pred?)
		(car (subst-leftmost-rec new old slist equality-pred? #f '() '()))))


(trace-define subst-leftmost-rec
	(lambda (new old slist equality-pred? switched? current)
		(cond [(null? slist) (list current switched?)]
			  [(symbol? (car slist))
			   (if (and (equality-pred? (car slist) old)
			   	        (not switched?))
			   	   (append current (list new) (cdr slist)) 
			   	   ; (subst-leftmost-rec new old (cdr slist) equality-pred? #t (append current (list new)))
				   (subst-leftmost-rec new old (cdr slist) equality-pred? switched? (append current (list (car slist))) (cddr slist)))]
			  [else
			  (let ([rec (subst-leftmost-rec new old (car slist) equality-pred? switched? '() )])
				   (if switched?
				   	   (subst-leftmost-rec new old (cdr slist) equality-pred? switched? (append current (list (car slist))))
				       (subst-leftmost-rec new old (cdr slist) 
				       	                           equality-pred? 
				       	                           (cadr rec) 
				       	                           (append current 
				       	                                   (list (car rec))))))])))
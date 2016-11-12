;;Xiwen Li
;;Assignment 06


;;1
(define curry2
	(lambda (x)
		(lambda (y)
			(lambda (z)
				(x y z)))))

;;2
(define curried-compose
	(lambda (proc1)
		(lambda (proc2)
			(lambda (obj)
				(proc1 (proc2 obj)))))) 


;;3
(define compose
	(lambda list-of-functions
		(lambda (obj)
			(compose-rec (reverse list-of-functions) obj))))

(define compose-rec
	(lambda (functions obj)
		(if (null? (cdr functions))
			((car functions) obj)
			(compose-rec (cdr functions) 
					     ((car functions) obj)))))

;;4
(define make-list-c
	(lambda (n)
		(lambda (element)
			(let in-list ([index 0])
				(if (= index n)
					'()
					(cons element (in-list (+ index 1))))))))

;;5
(define let->application
	(lambda (ls)
		(let ([para (map car (cadr ls))]
			  [body (caddr ls)]
			  [vals (map cadr (cadr ls))])
			 (cons (list 'lambda para body) vals))))


;;6
(define let*->let
	(lambda (ls)
		(let ([vars (map car (cadr ls))]
			  [body (caddr ls)]
			  [vals (map cadr (cadr ls))])
			(let*->let-rec vars vals body))))

(define let*->let-rec
	(lambda (vars vals body)
		(if (null? vars)
			body
			(list 'let 
				  (list (list (car vars) (car vals))) 
				  (let*->let-rec (cdr vars) 
				                 (cdr vals) body)))))

;;7
(define filter-in 
	(lambda (pred? lst)
		(cond [(null? lst) '()]
			  [(pred? (car lst)) 
			   (cons (car lst) 
			  		 (filter-in pred? (cdr lst)))]
			  [else (filter-in pred? (cdr lst))])))

(define filter-out 
	(lambda (pred? lst)
		(cond [(null? lst) '()]
			  [(pred? (car lst)) 
			   (filter-out pred? (cdr lst))]
			  [else (cons (car lst) 
			  			  (filter-out pred? (cdr lst)))])))

(define sort-list-of-symbols
	(lambda (ls)
		(map string->symbol (sort string<? (map symbol->string ls)))))

(define invert
	(lambda (ls)
		(if (null? ls)
			'()
			(cons (reverse (car ls)) (invert (cdr ls))))))

(define vector-index
	(lambda (pred vector)
		(vector-index-rec pred (vector->list vector) 0)))

(define vector-index-rec
	(lambda (pred ls index)
		(cond [(null? ls) #f]
			  [(pred (car ls)) index]
			  [else (vector-index-rec pred (cdr ls) (+ index 1))])))

(define ribassoc
	(lambda (s los v fail-value)
		(cond [(null? los) fail-value]
			  [(list-index s los 0) (vector-ref v (list-index s los 0))]
			  [else (ribassoc s (cdr los) v fail-value)])))

(define list-index
	(lambda (tar ls index)
		(cond [(null? ls) #f]
			  [(equal? tar (car ls)) index]
			  [else (list-index tar (cdr ls) (+ index 1))])))

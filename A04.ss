;;Xiwen Li
;;Assignment 04

;;1
(define multi-set?
	(lambda (obj)
		(multi-set?-rec obj '())))

(define member?
	(lambda (target ls)
		(cond [(null? ls) #f]
			  [(equal? (car ls) target) #t]
			  [else (member? target (cdr ls))])))


(define multi-set?-rec
	(lambda (obj prev)
		(cond 
			[(null? obj) #t]
			[(not (list? obj)) #f]
			[(not (list? (car obj))) #f]
			[(member? (caar obj) prev) #f]
			[(< (cadar obj) 0) #f]
			[else (and (multi-set-element? (car obj))
					   (multi-set?-rec (cdr obj) (cons (caar obj) prev)))])))

(define multi-set-element?
	(lambda (obj)
		(and (symbol? (car obj)) 
			 (number? (cadr obj)))))


;;2
(define ms-size
	(lambda (ms)
		(apply + (map cadr ms))))

;;3
(define matrix-ref
	(lambda (m row col)
		(list-ref (list-ref m row) col)))

;;4
(define matrix?
	(lambda (obj)
		(matrix?-rec obj 0)))
		
(define matrix?-rec
	(lambda (obj len)
		(cond [(null? obj) #t]
			  [(not (list? obj)) #f]
			  [(not (list? (car obj))) #f]
			  [(null? (list? (car obj))) #f]
			  [(zero? len) (and (matrix-element? (car obj)) 
			  			        (matrix?-rec (cdr obj) (length (car obj))))]
			  [(not (= (length (car obj)) len)) #f]
			  [else (and (matrix-element? (car obj)) 
			  	         (matrix?-rec (cdr obj) len))])))

(define matrix-element?
	(lambda (obj)
		(if (null? obj)
			#f
			(matrix-element?-helper obj))))

(define matrix-element?-helper
	(lambda (obj)
		(if (null? obj)
			#t
			(and (number? (car obj))
				 (matrix-element?-helper (cdr obj))))))
;;5
(define matrix-transpose
	(lambda (m)
		(if (null? (car m))
			'()
			(cons (map car m) (matrix-transpose (map cdr m))))))

;;6
(define last
	(lambda (ls)
		(if (null? (cdr ls))
			(car ls)
			(last (cdr ls)))))

;;7
(define all-but-last
	(lambda (lst)
		(if (null? (cdr lst))
			'()
			(cons (car lst) (all-but-last (cdr lst))))))
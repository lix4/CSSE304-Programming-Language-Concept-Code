;;Xiwen Li
;;Assignment 05

;;1
(define minimize-interval-list-combiner
	(lambda (set1 set2)
		(cond
			[(>= (cadr set1) (cadr set2)) set1]
			[(list (car set1) (cadr set2))])))

(define minimize-interval-list-intersect-checker
	(lambda (set1 set2)
		(>= (cadr set1) (car set2))))

(define minimize-interval-list-rec
	(lambda (ls)
		(cond
			[(null? (cdr ls)) ls]
			[(minimize-interval-list-intersect-checker (car ls) (cadr ls)) (minimize-interval-list-rec (cons (minimize-interval-list-combiner (car ls) (cadr ls)) (cddr ls)))]
			[else (cons (car ls) (minimize-interval-list-rec (cdr ls)))])))

(define minimize-interval-list-predicate
	(lambda (element1 element2) 
		(< (car element1) (car element2))))

(define minimize-interval-list
	(lambda (ls)
		(minimize-interval-list-rec (list-sort minimize-interval-list-predicate ls))))

;;2
(define exists?
	(lambda (pred ls)
		(cond [(null? ls) #f]
			  [(pred (car ls)) #t]
			  [else (exists? pred (cdr ls))])))

;;3
(define list-index
	(lambda (pred ls)
		(list-index-rec pred ls 0)))


(define list-index-rec
	(lambda (pred ls index)
		(cond [(null? ls) #f]
			  [(pred (car ls)) index]
			  [else (list-index-rec pred (cdr ls) (+ index 1))])))
;;4
(define pascal-triangle 
	(lambda (n)
		(if (< n 0)
			'()
			(pascal-triangle-column-create 0 n '((1)) '(1)))))
		

(define pascal-triangle-column-create
	(lambda (index n current-result prev-row)
		(let ([current-row (cons '1
			                      (pascal-triangle-row-create prev-row))])
			(if (= index n)
				current-result
				(pascal-triangle-column-create (+ index 1) 
											   n 
										       (cons current-row 
										       	     current-result)
										       current-row)))))

(define pascal-triangle-row-create
	(lambda (prev-row)
		(if (equal? '(1) prev-row)
			'(1)
			(cons (+ (car prev-row) 
				     (cadr prev-row))
				  (pascal-triangle-row-create (cdr prev-row))))))


;;5
(define product
	(lambda (set1 set2)
		(if (null? set1)
			'()
			(append (product-rec (car set1) set2) 
				  (product (cdr set1) set2)))))

(define product-rec
	(lambda (element set2)
		(if (null? set2)
			'()
			(cons (list element (car set2)) 
				  (product-rec element (cdr set2))))))

;;6
(define max-edges
	(lambda (n)
		(/ (* n (- n 1)) 2)))

;;7
(define complete?
	(lambda (G)
		(complete?-rec G (map car G))))

			
(define complete?-rec
	(lambda (G all)
		(cond [(null? G) #t]
			  [(not (set?-eq all (complete?-single (car G)))) #f]
			  [else (complete?-rec (cdr G) all)])))


(define complete?-single
	(lambda (g)
		(cons (car g) (cadr g))))


(define subset?
	(lambda (set1 set2)
		(if (null? set1)
			#t
			(and (member? (car set1) set2) 
				 (subset? (cdr set1) set2)))))
(define set?-eq
	(lambda (set1 set2)
		(and (subset? set1 set2) 
			 (subset? set2 set1))))

(define member?
	(lambda (target ls)
		(cond [(null? ls) #f]
			  [(equal? (car ls) target) #t]
			  [else (member? target (cdr ls))])))

;;8
(define complete
	(lambda (ls)
		(complete-save-all-rec ls ls)))

(define complete-save-all-rec
	(lambda (ls all)
		(if (null? ls)
			'()
			(cons (complete-each (car ls) all)
			  	  (complete-save-all-rec (cdr ls) all)))))
		
(define complete-each
	(lambda (first all-elements)
		(cons first 
			  (list (remove-first first all-elements)))))

;;9
(define replace
	(lambda (old new ls)
		(cond [(null? ls) '()]
			  [(equal? old (car ls)) (cons new (replace old new (cdr ls)))]
			  [else (cons (car ls) (replace old new (cdr ls)))])))


;;10
(define remove-first
	(lambda (element ls)
		(cond [(null? ls) ls]
			  [(equal? element (car ls)) (cdr ls)]
			  [else (cons (car ls) (remove-first element (cdr ls)))])))


;;11
(define remove-last
	(lambda (element ls)
		(reverse (remove-first element (reverse ls)))))
		

;;Xiwen Li
;;Assignment 08
(load "A08-test-code.ss")

;;1
;;(a)
(define slist-map
	(lambda (proc lst)
		(cond [(null? lst) '()]
			  [(list? (car lst)) 
			   (cons (slist-map proc (car lst))
			   	     (slist-map proc (cdr lst)))]
			  [else (cons (proc (car lst))
			  	          (slist-map proc (cdr lst)))])))

;;(b)
(define slist-reverse
	(lambda (slist)
		(cond [(null? slist) '()]
			  [(list? (car slist)) 
			   (append (slist-reverse (cdr slist))
			   		   (list (slist-reverse (car slist))))]
			  [else (append (slist-reverse (cdr slist)) 
			  	            (list (car slist)))])))

;;(c)
(define slist-paren-count
	(lambda (slist)
		(+ 2 (let count-rec ([slist slist])
			 	(cond [(null? slist) 0]
				  	  [(list? (car slist)) 
				       (+ 2 (count-rec (car slist)) 
				       	    (count-rec (cdr slist)))]
				      [else (count-rec (cdr slist))])))))

;;(d)
(define slist-depth
	(lambda (slist)
		(let compare ([slist slist]
			          [index 1]
			          [max-depth 1])
				(cond [(null? slist)
					   (if (> max-depth index)
					   	   max-depth
					   	   index)]
				       [(symbol? (car slist)) (compare (cdr slist) index max-depth)]
				       [else (if (> (compare (car slist) (+ index 1) max-depth) max-depth)
				       	         (compare (cdr slist) index (compare (car slist) (+ index 1) max-depth))
				       	         (compare (cdr slist) index max-depth))]))))


(define slist-depth-rec
	(lambda (slist depth)
		(cond [(null? slist) depth]
			  [(symbol? (car slist)) 
			   (list '0 (slist-depth-rec (cdr slist) depth))]
			  [else (slist-depth-rec (cdr slist) (+ depth 1))])))

;;(e)
(define slist-symbols-at-depth
	(lambda (slist depth)
		(slist-symbols-at-depth-rec slist depth 1)))

(define slist-symbols-at-depth-rec
	(lambda (slist depth index)
		(cond [(null? slist) '()]
			  [(= depth index) 
			   (if (symbol? (car slist))
			   	   (cons (car slist) (slist-symbols-at-depth-rec (cdr slist) depth index))
			   	   (slist-symbols-at-depth-rec (cdr slist) depth index))]
			  [else (if (symbol? (car slist))
			  			(slist-symbols-at-depth-rec (cdr slist) depth index)
			  			(append (slist-symbols-at-depth-rec (car slist) depth (+ 1 index))
			  				    (slist-symbols-at-depth-rec (cdr slist) depth index)))])))

;;2
(define group-by-two
	(lambda (ls)
		(cond [(null? ls) '()]
			  [(null? (cdr ls)) (list ls)]
			  [else (cons (list (car ls) (cadr ls))
			  	          (group-by-two (cddr ls)))])))

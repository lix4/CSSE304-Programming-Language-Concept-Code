(define-datatype kcontinuation kcontinuation?
	[init-k]
	[flatten-cdr-k (ls list?)
	               (k continuation?)]
	[flatten-car-k (flatten-cdr list?) (k kcontinuation?)]
	[append-k (car-L1 k) ()])

(define apply-k
	(lambda (k v)
		(if (continuation? k)
			(cases continuation k
				[init-k ()
					(pretty-print v)
					(read-flatten-print))]
			    [flatten-cdr-k (ls k)
			    (if (list? (cdr k))
			    	(flatten-cps (car ls) 
			    		    (flatten-car-k v k)
			    		(lambda (u) (append-cps u v k)))
			        (apply-k k (cons (car ls) v)))]
			    [flatten-car-k (flatten-cdr k)
			      (append-cps u (flatten-cdr k))]
			    [append-k (car-L1 k)
			              (apply-k k (cons car-L1 v))]
			    [else "should not happen"]
			(k v))))

(define flatten-cps
	(lambda (ls k)
		(if (null? ls)
			(apply-k k ls)
			(flatten-cps (cdr ls)
				         (flatten-cdr-k ls k)))))


					    
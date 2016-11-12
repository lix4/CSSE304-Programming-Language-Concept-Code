(load "chez-init.ss")


;Xiwen Li
;Assignment 19
(define L1)
(define L2)
(define L)
(define k)
(define v)

(define-datatype kontinuation kontinuation?
  [init-k]
  [flatten-cdr-k (ls list?) 
                 (k kontinuation?)]
  [flatten-car-k  (flattened-cdr list?)
		          (K1 kontinuation?)]
  [append-k (car-L1 symbol?) 
            (K1 kontinuation?)])

(define apply-k 
  (lambda () ;(k v)
	 (cases kontinuation k
	    [init-k () v]
	    [flatten-cdr-k (l1 kk)
				       (if (list? (car l1))
				       	   (begin (set! L (car l1)) 
				       	   	      (set! k (flatten-car-k v kk))
				       	   	      (flatten-cps))
				       	   (begin (set! k kk)
				       	          (set! v (cons (car l1) v))
				       	          (apply-k)))]	
	    [flatten-car-k (flattened-cdr K1)
	                   (begin (set! L1 v)
	                   		  (set! L2 flattened-cdr)
	                   		  (set! k K1)
	                   		  (append-cps))]
	    [append-k (car-L1 K1)
		          (begin (set! v (cons car-L1 v))
		          	 	 (set! k K1)
		          	 	 (apply-k))])))

(define append-cps 
  (lambda ()
    (if (null? L1)
    	(begin (set! v L2)
    		   (apply-k))
        (begin (set! k (append-k (car L1) k))
        	   (set! L1 (cdr L1))
        	   (append-cps)))))


(define read-flatten-print
  (lambda ()
    (display "enter slist to flatten: ")
    (let ([slist (read)])
      (unless (eq? slist 'exit)
      	 (begin (set! L slist)
      	 	    (set! k (init-k))
      	 	    (flatten-cps))))))



(define flatten-cps
  (lambda () ;ls k
    (if (null? ls)
    	(begin (set! v L) 
    		   (apply-k))
    	(begin (set! k (flatten-cdr-k L k))
    		   (set! L (cdr L)) 
    		   (flatten-cps)))))

(trace append-cps flatten-cps apply-k)

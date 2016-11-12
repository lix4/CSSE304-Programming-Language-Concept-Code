(load "chez-init.ss")

;;Xiwen Li
;;Assignment 19

(define ls)
(define L1)
(define L2)
(define v)
(define k)

(define-datatype kontinuation kontinuation?
  [init-k]
  [flatten-cdr-k (ls list?) (k kontinuation?)]
  [flatten-car-k  (flattened-cdr list?)
		          (k kontinuation?)]
  [append-k (car-L1 symbol?) (k kontinuation?)])

(define apply-k 
  (lambda ()
	 (cases kontinuation k
	    [init-k () v]
	    [flatten-cdr-k (ls1 K1)
	       (if (list? (car ls1))
	       	   (begin (set! ls (car ls1))
	       	   		  (set! k (flatten-car-k v K1))
	       	   		  (flatten-cps))
	       	   (begin (set! k K1)
	       	   	      (set! v (cons (car ls1) v))
	       	   	      (apply-k)))]
	    [flatten-car-k (flattened-cdr K1)
	    			   (begin (set! L1 v)
	    			   	      (set! L2 flattened-cdr)
	    			   	      (set! k K1)
	    			   	      (append-cps))]
	    [append-k (car-L1 K1)
	              (begin (set! k K1)
	              	     (set! v (cons car-L1 v))
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
      	    (begin (set! k (init-k))
      	    	   (set! ls slist)
      	    	   (flatten-cps))))))

(define flatten-cps
  (lambda ();(ls k)
    (if (null? ls)
	    (begin (set! v ls)
	    	   (apply-k))
	    (begin (set! k (flatten-cdr-k ls k))
	    	   (set! ls (cdr ls))
	    	   (flatten-cps)))))


(trace append-cps flatten-cps apply-k)

			



(define-datatype continuation continuation?
	[append-k (a any?)
	          (k continuation?)]
	[init-k]
	[revl-k (L any?) (k continuation?)]
	[rev2-k (reversed-cdr (list-of any?)) (k continuation?)])

(define trace-it
	(lambda (proc-name)
		(when *tracing* 
			(printf "~a" sym)
			(printf "L=~s" L)
			(printf " a=~s" a))))



(define apply-k 
	(lambda () ;(k v)
		(cases continuation k
			[init-k () (printf "answer: ~s~n" v)]
			[append-k (a1 k1) 
			          (set! v (cons (car a1) v))
			          (set! k k1)
		        	  (apply-k)]
			[revl-k (L k1)
			(if (pair? L)
				(begin (set! L (cdr L)) (set! k (rev2-k L k1)) (reverse*-cps))
				(begin (set! a v) (set! b (list (car L))) (set! k k1) (reverse*-cps)))]
			[rev2-k (reversed-cdr k1)
					(set! a reversed-cdr) 
					(set! b (list v))
					(set! k k1)
			        (append*-cps)])))


(define L)
(define K)
(define v)
(define a)
(define b)
(define k)



(define reverse*-cps
	(lambda () ;L k
		(if (null? L)
			(begin (set! v '()) (apply-k))
			(begin (set! L (cdr L)) 
				   (set! k (revl-k L K)) 
				   (reverse*-cps)))))

(define append*-cps
	(lambda ()
		(if (null? L)
			(begin (set! v '()) (apply-k))
			(begin (set! k (append-k)) (set! a (cdr a)) (append*-cps)))))

(define test
	(lambda ()
		(set! k (init-k))
		(set! L '(a  ((b c) () (((d))))))
		(reverse*-cps)))
		; (reverse*-cps '(a  ((b c) () (((d))))))))
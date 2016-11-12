(define vector-list
	(lambda (exp)
		(let ([expr '()])
			(set! expr (vector-copy exp))
			(lambda (msg . args)
				(case msg
					[(size) (vector-length expr)]
					[(get) (vector-ref expr (car args))]
					[(set) (vector-set! expr (car args) (cadr args))]
					[(current) expr]
					[(add) (let ([vlist (vector->list expr)])
								(set! vlist (append vlist (list (car args))))
								(set! expr (list->vector vlist)))]
					[(remove) (let ([temp (vector->list expr)])
								   (set! expr (reverse (set-car! (reverse temp) '())))
								   (set! expr (vector->list expr)))]
					[(capacity) (vector-length expr)])))))
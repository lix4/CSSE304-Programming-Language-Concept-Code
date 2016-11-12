(let ([a 5] [b 6])
	(let ([c (+ a b)])
		(* a b c)
		)
	)

(define fact
	(lambda (n)
		(if (zero? n)
			1
			(* n (fact (- n 1)))
			)
		)
	)

;;The tail recursion doesn't let the stack to grow.
(define fact-tail
	(lambda	(n accum)
		(if (zero? n)
			accum
			(fact-tail (- n 1) (* n accum))
			)
		)
	)

(define fact2
	(lambda (n)
		(fact-tail n 1)
		)
	)


(define make-adder
	(lambda (n)
		(lambda (m)
			(+ m n)
			)
		)
	)

(define add5 (make-adder 5))

((make-adder 8) 5)

(((lambda (n) (lambda (a) (+ n a))) 5) 4)
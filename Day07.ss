(define largest-in-lists
	(lambda (ls)
		(letrec ([flatten (lambda (lst)
							(cond [(null? lst) '()]
								  [(number? (car lst)) (cons (car lst) (flatten (cdr lst)))]
								  [(list? (car lst)) (append (car lst) (flatten (cdr lst)))]
								  [else (cons (flatten (car lst)) (flatten (cdr lst)))]
								))])
		(let ([sorted (list-sort > (flatten ls))])
			(if (null? sorted)
				#f
				(car sorted))))))
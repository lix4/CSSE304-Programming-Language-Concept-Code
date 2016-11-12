
(define make-list-iterator
	(lambda (ls)
		(let ([mlist (list-copy ls)])
				(letrec ([my-iterator 
						(lambda (msg)
							(if (equal? msg 'next)
								(if (null? mlist)
									#f
									(let ([temp (car mlist)])
										(begin (set! mlist (cdr mlist)) temp)))))])
				my-iterator))))

(define flatten
	(lambda (slist)
		(cond [(null? slist) '()]
			  [(symbol? (car slist)) (cons (car slist) (flatten (cdr slist)))]
			  [else (append (flatten (car slist)) (flatten (cdr slist)))])))


(define make-slist-leaf-iterator
	(lambda (slist)
		(let* ([slist (flatten (list-copy slist))]
			   [my-iterator (make-list-iterator slist)])
		my-iterator)))

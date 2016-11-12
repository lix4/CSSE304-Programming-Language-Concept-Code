(define range-of-numbers
	(lambda (lon)
		(- (range-of-numbers-largest lon (car lon))
		   (range-of-numbers-smallest lon (car lon)))))


(define range-of-numbers-largest 
	(lambda (lon largest)
		(cond [(null? lon) largest]
			  [(> (car lon) largest) (range-of-numbers-largest (cdr lon) (car lon))]
			  [else (range-of-numbers-largest (cdr lon) largest)])))
		
(define range-of-numbers-smallest
	(lambda (lon smallest)
		(cond [(null? lon) smallest]
			  [(< (car lon) smallest) (range-of-numbers-smallest (cdr lon) (car lon))]
			  [else (range-of-numbers-smallest (cdr lon) smallest)])))


(define symmetric?
	(lambda (rel)
		(symmetric?-rec rel rel)))

(define symmetric?-rec
	(lambda (rel rel-save)
		(cond [(null? rel) #t]
			  [(equal? (caar rel)
			  	       (cadar rel))
			   (and #t (symmetric?-rec (cdr rel) rel-save))]
			  [else (and (member? (list (cadar rel) (caar rel)) rel-save) (symmetric?-rec (cdr rel) rel-save))])))


(define member?
	(lambda (target ls)
		(cond [(null? ls) #f]
			  [(equal? (car ls) target) #t]
			  [else (member? target (cdr ls))])))

(define los->ms
	(lambda (ls)
		(los-ms-rec (sort-list-of-symbols ls) '() 0)))


(define los-ms-rec
	(lambda (new-ls last count)
		(cond [(null? new-ls) (list (list last (- count 1)))]
			  [(null? last) (los-ms-rec new-ls (car new-ls) (+ count 1))]
			  [(equal? last (car new-ls)) (los-ms-rec (cdr new-ls) last (+ count 1))]
			  [else (append (list (list last (- count 1))) (los-ms-rec new-ls '() 0))])))


(define sort-list-of-symbols
	(lambda (ls)
		(map string->symbol (sort string<? (map symbol->string ls)))))


(define symbols-with-n-count
	(lambda (los n)
		(symbols-with-n-count-rec (los->ms los) n)))

(define symbols-with-n-count-rec
	(lambda (ls n)
		(cond [(null? ls) 0]
			  [(= (cadar ls) n) (+ 1 (symbols-with-n-count-rec (cdr ls) n))]
			  [else (symbols-with-n-count-rec (cdr ls) n)])))
(trace-define contains-both?
	(lambda (lst sym1 sym2)
		(and (member? sym1 lst) (member? sym2 lst))))

(trace-define member?
	(lambda (target ls)
		(cond [(null? ls) #f]
			  [(equal? (car ls) target) #t]
			  [else (member? target (cdr ls))])))



(define matrix
	(lambda (mat1 mat2)
		(map matrix-helper mat1 mat2)))

(define matrix-helper
	(lambda (ls1 ls2)
		(map + ls1 ls2)))

(define make-vec-iterator
	(lambda (vec)
		(let rec ([index 0])
			(lambda (msg . args)
					(case msg
						  [(val) (vector-ref vec index)]
						  [(set-val!) (vector-set! vec index args)]
						  [(next) index]
						  [(prev) (rec (- index 1)) (vector-ref vec index)])))))

(define make-c....r
	(lambda (str)
		(let ([proc-list (make-c....r (string->list str))])		
			(lambda (ls)
				(((car proc-list) (apply make-c....r (list->string (cdr proc-list))))) ls))))

(define compose
 (case-lambda
 [() (lambda (x) x)]
 [(first . rest)
 (let ([composed-rest (apply compose rest)])
 (lambda (x) (first (composed-rest x))))]))

(define make-c....r-converter
	(lambda (ls)
		(cond [(null? ls) '()]
			  [(equal? #\a (car ls)) (cons car (make-c....r-converter (cdr ls)))]
			  [(equal? #\d (car ls)) (cons cdr (make-c....r-converter (cdr ls)))])))


; (define pascal-triangle
;  (lambda (n)
;  (cond [(< n 0) '()]
;  [(= n 0) '((1))]
;  [else (let ([triangle-n-1 (pascal-triangle (- n 1))])
;  (cons (cons 1 (row-helper (car triangle-n-1)))
;  triangle-n-1))])))

; (define row-helper
; 	 (lambda (prev-row)
; 		 (if (null? (cdr prev-row))

; 		 	 ((lambda (x . y) 

; 		 	 	) 
; 		 	 	prev-row)

; 		 	 '(1)
; 			 (cons (+ (car prev-row) (cadr prev-row))
; 			 	   (row-helper (cdr prev-row))))))
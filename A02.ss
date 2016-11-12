;;Xiwen Li
;;Assignment02

;;1(a)
(define fact
	(lambda (x)
		(if (zero? x)
			1
			(* x (fact (- x 1))))))

;;(b)
(define choose
	(lambda (n k)
		(/ (fact n) (* (fact (- n k)) (fact k)))))

;;2
(define range
	(lambda	(m n)
		(if (>= m n)
			'()
			(cons m (range (+ m 1) n)))))

;;3
(define set?
	(lambda (ls)
		(if (null? ls)
			#t
			(if (set?-rec (car ls) (cdr ls))
				(set? (cdr ls))
				#f))))

(define set?-rec
	(lambda (tar ls)
		(cond [(null? ls) #t]
			  [(equal? tar (car ls)) #f]
			  [else (set?-rec tar (cdr ls))])))


;;4
(define sum-of-squares
	(lambda (lon)
		(if (null? lon)
			0
			(+ (* (car lon) (car lon)) (sum-of-squares (cdr lon))))))

;;5
(define make-vec-from-points
	(lambda (pt1 pt2)
		(list (- (car pt2) (car pt1)) (- (cadr pt2) (cadr pt1)) (- (caddr pt2) (caddr pt1)))))

;;6
(define dot-product
	(lambda (vec1 vec2)
		(+ (* (car vec1) (car vec2)) (* (cadr vec1) (cadr vec2)) (* (caddr vec1) (caddr vec2)))))

;;7
(define vec-length
	(lambda (v)
		(sqrt (sum-of-squares v))))

;;8
(define distance
	(lambda (p1 p2)
		(vec-length (make-vec-from-points p1 p2))))

;;9
(define cross-product
	(lambda (v1 v2)
		(list (- (* (cadr v1) (caddr v2)) (* (cadr v2) (caddr v1))) 
			  (- (* (caddr v1) (car v2)) (* (caddr v2) (car v1))) 
			  (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2))))))

;;10
(define parallel?
	(lambda (v1 v2)
		(equal? (cross-product v1 v2) '(0 0 0))))

;;11
(define collinear?
	(lambda (v1 v2 v3)
		(parallel? (make-vec-from-points v1 v2) (make-vec-from-points v2 v3))))
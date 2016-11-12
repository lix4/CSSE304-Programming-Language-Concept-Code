;;Xiwen Li
;;Assignment 03


;;1
(define nearest-point
	(lambda (pt ls)
		(nearest-point-rec pt ls (car ls))))

(define nearest-point-rec
	(lambda (pt ls nearest)
		(cond [(null? ls) nearest]
			  [(< (3D-distance pt (car ls)) (3D-distance pt nearest)) (nearest-point-rec pt (cdr ls) (car ls))]
			  [else (nearest-point-rec pt (cdr ls) nearest)])))

(define 3D-distance
	(lambda (p1 p2)
		(sqrt (+ (square (- (car p1) (car p2))) 
			  (square (- (cadr p1) (cadr p2))) 
			  (square (- (caddr p1) (caddr p2)))))))

(define square
	(lambda (n)
		(* n n)))

;;2
(define union
	(lambda (set1 set2)
		(cond [(null? set2) '()]
			  [(member? (car set2) set1) (union set1 (cdr set2))]
			  [else (cons set1 (union ))]

(define member?
	(lambda (target ls)
		(cond [(null? ls) #f]
			  [(equal? (car ls) target) #t]
			  [else (member? target (cdr ls))])))

;;3
(define intersection
	(lambda (intersection1 intersection2)
		
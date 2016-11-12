;;Xiwen Li
;;Assignment 01

;;1
(define Fahrenheit->Celsius
	(lambda (x)
		(* (/ 5 9) (- x 32))
		)
	)


;;2
(define interval-contains?
	(lambda (interval tar)
		(and (>= tar (car interval)) (<= tar (cadr interval)))
		)
	)

;;3
(define interval-intersects?
	(lambda (i1 i2)
		(or (interval-contains? i1 (car i2)) (interval-contains? i1 (cadr i2)) (and (<= (car i1) (car i2)) (>= (cadr i1) (cadr i2))) (and (<= (car i2) (car i1)) (>= (cadr i2) (cadr i1)))   )
		)
	)

;;4
(define interval-union
	(lambda (set1 set2)
		(if (interval-intersects? set1 set2)
			(list (list (min (car set1) (car set2) (cadr set1) (cadr set2)) (max (car set1) (car set2) (cadr set1) (cadr set2))))
			(list set1 set2)
			)
		)
	)

;;5
(define divisible-by-7?
	(lambda (x)
		(zero? (remainder x 7))
		)
	)

;;6
(define ends-with-7?
	(lambda (num)
		(= 7 (remainder num 10))
		)
	)


;;7
(define 1st car)

(define 2nd cadr)

(define 3rd caddr)

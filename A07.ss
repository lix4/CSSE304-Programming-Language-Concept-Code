;;Xiwen Li
;;Assignment 07

;;1
; (define
(define vector-append-list
  (lambda (vec ls)
    (let ([new-vector (make-vector (+ (vector-length vec) (length ls)))])
      (copy-from-vector new-vector vec 0)
      (copy-from-list new-vector ls (vector-length vec) (vector-length vec))
      new-vector)))

(define copy-from-vector
	(lambda (new-vector vec index) 
		(cond
			[(equal? vec '#()) new-vector]
			[(equal? index (- (vector-length vec) 1)) (vector-set! new-vector index (vector-ref vec index))]
			[else 
				(vector-set! new-vector index (vector-ref vec index))
				(copy-from-vector new-vector vec (+ index 1))])))

(define copy-from-list
	(lambda (new-vector ls index vec-length) 
		(cond
			[(null? ls) new-vector]
			[(equal? index (- (vector-length new-vector) 1)) (vector-set! new-vector index (list-ref ls (- index vec-length)))]
			[else 
				(vector-set! new-vector index (list-ref ls (- index vec-length)))
				(copy-from-list new-vector ls (+ index 1) vec-length)])))

;;2
(define qsort
	(lambda (pred ls)
		(cond
			[(null? ls) '()]
			[else (let ((pivot (car ls)))
			(append	(qsort pred (qsort-part (lambda (a) (pred a pivot)) (cdr ls))) 
				    (list pivot) 
				    (qsort pred (qsort-part (lambda (a) (not (pred a pivot))) (cdr ls)))))])))

(define qsort-part
	(lambda (procedure ls)
		(cond 
			[(null? ls) '()]
			[(procedure (car ls)) (append (list (car ls)) (qsort-part procedure (cdr ls)))]
			[else (append (qsort-part procedure (cdr ls)))])))

(define connected?
	(lambda (graph)
		(cond
			[(equal? (length graph) 1) #t]
			[else (connected?-rec (list-sort connected?-sort-predicate graph) '())]
			)
		)
	)


(define connected?-rec
	(lambda (graph union)
		(cond
			[(null? graph) #t]
			[else (let ((current-union (append (list (caar graph)) (cadar graph))))
				(cond
					[(equal? union '()) (connected?-rec (cdr graph) current-union)]
					[(not (connected?-intercept current-union union)) #f]
					[else (connected?-rec (cdr graph) (connected?-combiner union current-union))]
					)
				)])))

(define connected?-sort-predicate
	(lambda (ls1 ls2)
		(string<? (symbol->string (car ls1)) (symbol->string (car ls2)))))

(define connected?-combiner
	(lambda (ls1 ls2)
		(cond
			[(null? ls2) ls1]
			[(member (car ls2) ls1) (connected?-combiner ls1 (cdr ls2))]
			[else (connected?-combiner (append ls1 (list (car ls2))) (cdr ls2))])))

(define connected?-intercept
	(lambda (ls1 ls2)
		(cond
			[(null? ls1) #f]
			[(member (car ls1) ls2) #t]
			[else (connected?-intercept (cdr ls1) ls2)])))

;;4
(define reverse-it
	(lambda (lst)
		(let rec ([lst lst]
			      [new-lst '()])
			(if (null? lst)
				new-lst
				(rec (cdr lst) 
					 (cons (car lst) 
					 	   new-lst))))))

;;5
;(1)
(define empty-BST 
	(lambda empty
		'()))

;(2)
(define empty-BST?
	(lambda (BST)
		(null? BST)))

;;(3)
(define BST-insert
	(lambda (num bst)
		(cond [(null? bst) (list num '() '())]
			  [(= num (car bst)) bst]
			  [(< num (car bst)) (list (car bst) (BST-insert num (cadr bst)) (caddr bst))]
			  [else (list (car bst) (cadr bst) (BST-insert num (caddr bst)))])))

;;(4)
(define BST-inorder
	(lambda (bst)
		(if (null? bst)
			'()
			(append (BST-inorder (cadr bst)) 
				  	(list (car bst)) 
				  	(BST-inorder (caddr bst))))))

;;(5)
(define BST?
	(lambda (bst)
		(cond
			[(null? bst) #t]
			[(not (list? bst)) #f]
			[(not (number? (car bst))) #f]
			[(not (equal? (length bst) 3)) #f]
			[(not (or (list? (cadr bst)) (list? (caddr bst)))) #f]
			[(not (BST-check-value (BST-inorder bst))) #f]
			[else (and (BST? (cadr bst)) (BST? (caddr bst)))])))

(define BST-check-value
	(lambda (in-order)
		(cond
			[(null? in-order) #t]
			[(equal? (length in-order) 1) #t]
			[(> (car in-order) (cadr in-order)) #f]
			[else (BST-check-value (cdr in-order))])))

;;(6)
(define BST-element car)

(define BST-left cadr)

(define BST-right caddr)

;;(7)
(define BST-insert-nodes
	(lambda (bst nums)
		(cond [(null? nums) bst]
			  [else (BST-insert-nodes (BST-insert (car nums) bst)
				                      (cdr nums))])))

;;(8)
(define BST-contains?
	(lambda (bst num)
		(cond [(null? bst) #f]
			  [(= num (car bst)) #t]
			  [(< num (car bst)) (BST-contains? (cadr bst) num)]
			  [else (BST-contains? (caddr bst) num)])))

;;5
(define map-by-position 
	(lambda (fn-list arg-list)
		(map apply fn-list (map list arg-list))))

(define bt-leaf-sum
	(lambda (ls)
		(cond
			[(number? ls) ls]
			[else (+ (bt-leaf-sum (cadr ls)) 
				     (bt-leaf-sum (caddr ls)))])))

(define bt-inorder-list
	(lambda (bt)
		(cond
			[(number? bt) '()]
			[else (append (bt-inorder-list (cadr bt)) (list (car bt)) (bt-inorder-list (caddr bt)))])))


(define bt-max
	(lambda (bt)
		(cond
			[(number? bt) bt]
			[(< (bt-max (cadr bt)) (bt-max (caddr bt))) (bt-max (caddr bt))]
			[else (bt-max (cadr bt))])))

(define bt-max-interior
	(lambda (bt)
			(car (bt-max-interior-rec bt))))

(define bt-max-interior-rec
	(lambda (bt)
			(cond
				[(and (number? (cadr bt)) (number? (caddr bt))) (list (car bt) (max (cadr bt) (caddr bt) (+ (cadr bt) (caddr bt))) (+ (cadr bt) (caddr bt)))]
				[(number? (caddr bt)) 
					(let ((left-tree-info (bt-max-interior-rec (cadr bt))) (right-value (caddr bt)))
						(let ((current-sum (+ (caddr left-tree-info) right-value)))
							(cond
								[(equal? (max current-sum (cadr left-tree-info)) current-sum) (list (car bt) current-sum current-sum)]
								[(equal? (max current-sum (cadr left-tree-info)) (cadr left-tree-info)) (list (car left-tree-info) (cadr left-tree-info) current-sum)]
								)	
							)
						)]
				[(number? (cadr bt)) 
					(let ((right-tree-info (bt-max-interior-rec (caddr bt))) (left-value (cadr bt)))
						(let ((current-sum (+ (caddr right-tree-info) left-value)))
							(cond
								[(>= current-sum (cadr right-tree-info)) (list (car bt) current-sum current-sum)]
								[else (list (car right-tree-info) (cadr right-tree-info) current-sum)]
								)	
							)
						)]
				[else (let ((left-tree-info (bt-max-interior-rec (cadr bt))) (right-tree-info (bt-max-interior-rec (caddr bt)))) 
							(let ((current-sum (+ (caddr left-tree-info) (caddr right-tree-info))))
									 (cond
									 	[(equal? (max (cadr right-tree-info) current-sum (cadr left-tree-info)) current-sum) (list (car bt) current-sum current-sum)]
										[(equal? (max (cadr right-tree-info) current-sum (cadr left-tree-info)) (cadr left-tree-info)) (list (car left-tree-info) (cadr left-tree-info) current-sum)]
										[(equal? (max (cadr right-tree-info) current-sum (cadr left-tree-info)) (cadr right-tree-info)) (list (car right-tree-info) (cadr right-tree-info) current-sum)]
										)
									)
								
						)]	
			)
		)
	)
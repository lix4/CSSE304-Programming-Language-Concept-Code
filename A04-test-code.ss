
(define (test-multi-set?)
  (let ([correct '(#t #t #t #t #t #t #t #t)]
        [answers 
          (list
	   (and (multi-set? '())
		(not (multi-set? '(a b))))
	   (and (multi-set? '((a 2)))
		(not (multi-set? '((a -1)))))
	   (and (multi-set? '((a 2)(b 3)))
		(not (multi-set? '((a 2) (a 3)))))
	   (and (not (multi-set? '(a b))) 
		(not (multi-set? '((a 3) b))))
	   (and (not (multi-set? '((a 2) (2 3))))
		(multi-set? '((a 2)))
		(not (multi-set? '(#(a 3)))))
	   (and (multi-set? '())
		(not (multi-set? '((a 3) b))))
	   (and (multi-set? '())
		(not (multi-set? 5)))
	   (and (multi-set? '())
		(not (multi-set? (list (cons 'a 2))))
		(not (multi-set? '((a 2) (a 3)))))
	   )])
    (display-results correct answers equal?)))

(define (test-ms-size)
  (let ([correct '(0 2 5)]
        [answers 
          (list
	   (ms-size '())
	   (ms-size '((a 2)))
	   (ms-size '((a 2)(b 3)))
	   )])
    (display-results correct answers equal?)))

(define (test-matrix-ref)
  (let ([correct '(2 3 5)]
        [answers 
          (list
	   (matrix-ref '((1 2 3 4 5) (4 3 2 1 5) (5 4 3 2 1))  2 3)
	   (matrix-ref '((1 2 3 4) (4 3 2 1))  1 1)
	   (matrix-ref '((1 2 3 4 5) (4 3 2 1 5) (5 4 3 2 1))  0 4)
          )])
    (display-results correct answers equal?)))

(define (test-matrix?)
  (let ([correct '(#f #f #f #t #f #f #t #f)]
        [answers 
	 (list
	  (matrix? 5)
	  (matrix? "matrix")
	  (matrix? '(1 2 3))
	  (matrix? '((1 2 3)(4 5 6)))
	  (matrix? '#((1 2 3)(4 5 6)))
	  (matrix? '((1 2 3)(4 5 6)(7 8)))
	  (matrix? '((1)))
	  (matrix? '(()()()))
	 )])
    (display-results correct answers equal?)))

            


(define (test-matrix-transpose)
  (let ([correct '(((1 4) (2 5) (3 6)) ((1) (2) (3)) ((1 2 3)))]
        [answers 
          (list
	   (matrix-transpose '((1 2 3) (4 5 6)))
	   (matrix-transpose '((1 2 3)))
	   (matrix-transpose '((1) (2) (3)))
	  )])
    (display-results correct answers equal?)))

(define (test-last)
  (let ([correct '( 4 c (()()))]
        [answers 
          (list
	   (last '(1 5 2 4))
	   (last '(c)) 
	   (last '(() (()) (()())))
	  )])
    (display-results correct answers equal?)))

(define (test-all-but-last)
  (let ([correct '((1 5 2) () (() (())))]
        [answers 
          (list
	   (all-but-last '(1 5 2 4))
	   (all-but-last '(c)) 
	   (all-but-last '(() (()) (()())))	   
	  )])
    (display-results correct answers equal?)))


;;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
     (display ": ")
     (pretty-print 
      (if (andmap test-procedure? correct results)
          'All-correct
          `(correct: ,correct yours: ,results)))))

(define sequal?-grading
  (lambda (l1 l2)
    (cond
     ((null? l1) (null? l2))
     ((null? l2) (null? l1))
     ((or (not (set?-grading l1))
          (not (set?-grading l2)))
      #f)
     ((member (car l1) l2) (sequal?-grading
                            (cdr l1)
                            (rember-grading
                             (car l1)
                             l2)))
     (else #f))))

(define set?-grading
  (lambda (s)
    (cond [(null? s) #t]
          [(not (list? s)) #f]
          [(member (car s) (cdr s)) #f]
          [else (set?-grading (cdr s))])))

(define rember-grading
  (lambda (a ls)
    (cond
     ((null? ls) ls)
     ((equal? a (car ls)) (cdr ls))
     (else (cons (car ls) (rember-grading a (cdr ls)))))))

(define set-equals? sequal?-grading)




;; You can run the tests individually, or run them all
;; by loading this file (and your solution) and typing (r)

(define (run-all)
  (display 'multi-set?) 
  (test-multi-set?)
  (display 'ms-size)
  (test-ms-size)
  (display 'matrix-ref) 
  (test-matrix-ref)
  (display 'matrix?) 
  (test-matrix?)    
  (display 'matrix-transpose) 
  (test-matrix-transpose)
  (display 'last) 
  (test-last)  
  (display 'all-but-last) 
  (test-all-but-last)  
  
)

(define r run-all)


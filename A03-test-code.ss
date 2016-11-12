(define (test-nearest-point)
  (let ([correct '((1 4 2) (1 4 4) (1 3 3) (2 6 13))]
        [answers 
          (list
	   (nearest-point '(1 4 3) '((1 5 2) (1 4 2) (1 6 13)))
	   (nearest-point '(1 4 3) '((1 4 4) (1 4 2) (1 6 13)))

	   (nearest-point '(1 4 3) '((1 3 2) (1 5 2) (1 6 13) (1 3 3) (1 4 2) ))
	   (nearest-point '(2 8 13) '((1 4 2) (1 5 2) (2 6 13)))
	  )])
    (display-results correct answers equal?)))

(define (test-union)
    (let ([correct '((a b c d e f g h j) (a b c d e) (a b c) ())]
          [answers 
            (list 
	     (union '(a b d e f h j) '(f c e g a))
	     (union '(a b c) '(d e))
	     (union '(a b c) '())
	     (union '() '())
            )])
    (display-results correct answers set-equals?)))

(define (test-intersection)
  (let ([correct '((i h) () () ())]
        [answers 
          (list
	   (intersection '(a b d e f h i j) '(h q r i z))
	   (intersection '(g h i) '(j k l))
	   (intersection '(a p t) '())
	   (intersection '() '(g e t))
	  )])
    (display-results correct answers set-equals?)))

(define (test-subset?)
  (let ([correct ' (#t #f #f #t #t #t #t)]
        [answers 
          (list
	    (subset? '(c b) '(a d b e c))
	    (subset? '(c b) '(a d b e))
	    (subset? '(c b) '())
	    (subset? '(c b) '(b c))
	    (subset? '(1 3 4) '(1 2 3 4 5))
	    (subset? '() '())
	    (subset? '() '(x y))
	  )])
    (display-results correct answers equal?)))

(define (test-relation?)
  (let ([correct '(#f #t #t #t #f #f #f #f )]
        [answers 
          (list
	   (relation? 5) 
	   (relation? '())
	   (relation? '((a b) (b c))) 
	   (relation? '((a b) (b a) (a a) (b b))) 
	   (relation? '((a b) (b c d))) 
	   (relation? '((a b) (c d) (a b))) 
	   (relation? '((a b) (c d) "5")) 
	   (relation? '((a b) . (b c))) 
	  )])
    (display-results correct answers equal?)))

(define (test-domain)
  (let ([correct '((2 3 1) ())]
        [answers 
          (list
	    (domain '((1 2) (3 4) (1 3) (2 7) (1 6)))
	    (domain '())
	  )])
    (display-results correct answers set-equals?)))

(define (test-reflexive?)
  (let ([correct '(#t #t #t #t)]
        [answers 
          (list
	   (reflexive? '((a a) (b b) (c d) (b c) (c c) (e e) (c a) (d d)))
           (not (reflexive? '((a a) (b b) (c d) (b c) (e e) (c a) (d d))))
           (not (reflexive? '((a a) (c d) (b c) (c c) (e e) (c a) (d d))))
	   (reflexive? '())
	   )])
    (display-results correct answers eq?)))

(define (test-hailstone-step-count)
  (let ([correct '(0 1 7 16 111 178)]
        [answers 
          (list
	   (hailstone-step-count 1)
	   (hailstone-step-count 2)
	   (hailstone-step-count 3)
	   (hailstone-step-count 7)
	   (hailstone-step-count 27)
	   (hailstone-step-count 871)

	   )])
    (display-results correct answers eq?)))

;;-----------------------------------------------

(define display-results
  (lambda (correct results test-procedure?)
     (display ": ")
     (pretty-print 
      (if (andmap test-procedure? correct results)
          'All-correct
          `(correct: ,correct yours: ,results)))))

(define set-equals?  ; are these list-of-symbols equal when
  (lambda (s1 s2)    ; treated as sets?
    (if (or (not (set? s1)) (not (set? s2)))
        #f
        (not (not (and (is-a-subset? s1 s2) (is-a-subset? s2 s1)))))))

(define is-a-subset?
  (lambda (s1 s2)
    (andmap (lambda (x) (member x s2))
      s1)))

(define set?
  (lambda (s)
    (cond [(null? s) #t]
          [(not (list? s)) #f]
          [(member (car s) (cdr s)) #f]
          [else (set? (cdr s))])))



;; You can run the tests individually, or run them all
;; by loading this file (and your solution) and typing (r)

(define (run-all)
  (display 'nearest-point) 
  (test-nearest-point)    
  (display 'union) 
  (test-union)
  (display 'intersection) 
  (test-intersection)
  (display 'subset?) 
  (test-subset?)  
  (display 'relation?) 
  (test-relation?)  
  (display 'domain) 
  (test-domain)  
  (display 'reflexive?) 
  (test-reflexive?)
  (display 'hailstone-step-count) 
  (test-hailstone-step-count)
 
)

(define r run-all)


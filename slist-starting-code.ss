; (define (contains? slist sym)
; 	(let in-list? ([slist slist])
; 		(cond [(null? slist) #f]
; 			  [(symbol? (car slist))
; 			   (or (eq? (car slist) sym)
; 			   	   (in-list? (cdr slist) sym))]
; 			  [else ;car is a slist.
; 			  	(or (in-list? (car slist) sym)
; 			  	    (in-list? (cdr slist) sym))])))

(contains? '() 'a)                          ; #f
(contains? '(b a) 'a)                       ; #t
(contains? '(( b (a)) ()) 'a)               ; #t
(contains? '((c b ()) ( b (c a)) ()) 'a)    ; #t
(contains? '((c b ()) ( b (c a)) ()) 'p)    ; #f


(define (count-occurrences slist sym)
	(let count ([slist slist])
		(cond [(null? slist) 0]
			  [(symbol? (car slist)) 
			   (+ (if (eq? (car slist) sym)
			   		  1
			   		  0)
			      (count (cdr slist)))]
			  [else (+ (count (car slist))
			  	       (count (cdr slist)))])))))



(count-occurrences '() 'a)                      ; 0
(count-occurrences '(b a b () a b) 'a)          ; 2
(count-occurrences '(b a b () a b) 'a)          ; 3
(count-occurrences '(b ((a) a) b () a b) 'a)    ; 3
(count-occurrences '((b ((a) a) b () a b)) 'a)  ; 3 



(define (notate-depth slist)
	(let notate ([slist slist]
		         [depth 0])
		(cond [(null? slist) depth]
			  [(symbol? (car slist)) 
			   (+ (list (car slist) depth)
			      	 (notate (cdr slist) depth))]
			  [else 
			  	(cons (notate (car slist) (+ 1 depth))
			  		  (notate (cdr slist) (+ 1 depth)))])))



(notate-depth '())                          ; ()
(notate-depth '(a))                         ; ((a 0)) 
(notate-depth '((a b) c))                   ; (((a 1) (b 1)) (c 0))
(notate-depth '( () (a (b)) c ((d) () e)))  ; (() ((a 1) ((b 2))) (c 0) (((d 2)) () (e 1)))
(notate-depth '((() (a (b)) c ((d) () e)))) ; ((() ((a 2) ((b 3))) (c 1) (((d 3)) () (e 2))))

(define  (flatten slist)




(flatten '( () (a ((b) c () ((d e ((f))) g) h)) ()))  ; (a b c d e f g h)



(define  (subst s1 s2 slist)
	(let subst ([slist slist])
		(cond [(null? slist) '()]
			  [(symbol? (car slist)) 
			   (cons (if (eq? (car slist) s1)
			   	   		 s2
			   	         (car slist))
			   	     (subst (cdr slist)))] 
			  [else (cons (subst (car slist))
			  	          (subst (cdr slist)))])))



(subst 'a 'b '(() a (c ((a) a) (c (((c a)))))))  ; (() b (c ((b) b) (c (((c b)))))
  

(define-syntax flip
	(syntax-rules ()
		((_ e) 
		 (let ([temp (reverse e)])
			   (set! e temp) e))))


(define-syntax swap!
	(syntax-rules ()
		((_ e1 e2)
	     (let ([temp e2])
	     	 (set! e2 e1)
	     	 (set! e1 temp)))))
	     	  
			 
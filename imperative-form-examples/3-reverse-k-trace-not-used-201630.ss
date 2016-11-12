;---- file 3-reverse-k-trace.ss ----
;---- Same as the previous program, but with tracing

(trace-define reverse*-k
  (lambda (L k)
    (if (null? L)
        (apply-continuation k '())
        (reverse*-k (cdr L)
                    (make-rev1 k L)))))

(trace-define append-k
  (lambda (a b k)
    (if (null? a)
        (apply-continuation k b)
        (append-k (cdr a)
		  b
		  (make-append-cont k a)))))

(trace-define make-rev1
  (lambda (k L)
    (trace-lambda rev1-k (reversed-cdr)
      (if (pair? (car L))
          (reverse*-k
	   (car L)
	   (make-rev2 reversed-cdr k))
          (append-k reversed-cdr
		    (list (car L))
		    k)))))

(trace-define make-rev2
  (lambda  (reversed-cdr k)
    (trace-lambda rev2-k (reversed-car)
      (append-k reversed-cdr
		(list reversed-car)
		k))))

(trace-define make-init-k
  (lambda ()
    (trace-lambda init-k (v)
      (display "answer: ")
      (display v)
      (newline))))

(trace-define make-append-cont
  (lambda (k a)
    (trace-lambda append-k (appended-cdr)
      (apply-continuation k
			  (cons (car a)
				appended-cdr)))))

(trace-define apply-continuation
  (lambda (k v) (k v)))

(trace-define testk
  (lambda () 
    (reverse*-k '(1 ((2 3) () 4))
		(make-init-k))))


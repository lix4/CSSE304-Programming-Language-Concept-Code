
;; Parsed expression datatypes

		
(define value?
  (lambda (n)
    (or (not (list? n))
        ((list-of number?) n))))
			
(define sym?
	(lambda(x)
		(cond
			[(null? x) #t]
			[(symbol? x) #t]
			[(list? x) (andmap sym? x)]
			[else #f])))

(define lambda?
			(lambda (x)
				(lambda (lis)
					(cond
							((null? lis) #t)
							((x lis) #t)
							(else (if (x (car lis))
												((lambda? x) (cdr lis))
												#f))))))
(define pair-of
  (lambda (pred)
    (letrec 
      ((func  
        (lambda (x) 
          (cond ((null? x) #t)
                ((pred x) #t)
                (else (if (pred (car x))
                          (func (cdr x))
                          #f)))))) 
    func)))
		
(define parameter?
  (pair-of 
    (lambda (x) 
      (or (symbol? x) (and (eq?  'ref (car x)) (symbol? (cadr x)))))))
		
(define-datatype expression expression?
	(var-exp
		(var symbol?))
	(lit-exp
		(lit value?))
	(vec-exp
		(vec vector?))
  (quote-exp
    (content (lambda (n) #t)))
	(if-exp
		(first expression?)
		(second expression?))
	(else-exp
		(first expression?)
		(second expression?)
		(third expression?))
	(lambda-exp
		(first parameter?)
		(body (list-of expression?)))
	(set!-exp
		(first symbol?)		
		(body expression?))
	(def-exp
    (first symbol?)
    (body expression?)) 
	(app-exp
		(first expression?)
		(rand (list-of expression?)))
)

	

	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))


	 
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))
	 
(define-datatype kontinuation kontinuation?
    [init-k]
    [else-k
        (then-exp expression?)
        (else-exp expression?)
        (env environment?)
        (k kontinuation?)]
    [if-k
        (then-exp expression?)
        (env environment?)
        (k kontinuation?)]
    [set!-k
        (env environment?)
        (dest symbol?)
        (k kontinuation?)]
    [def-k
        (dest symbol?)
        (k kontinuation?)]
    [rator-k
        (rands (list-of expression?))
        (env environment?)
        (k kontinuation?)]
    [rands-k
        (proc-val scheme-value?)
        (k kontinuation?)]
    [body-k
        (body (list-of expression?))
        (env environment?)
        (end-k kontinuation?)]
    [map-cdr-k
        (proc procedure?)
        (rest list?)
        (k kontinuation?)]
    [map-car-k
        (head scheme-value?)
        (k kontinuation?)]

    )
(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
   (parameter parameter?)
   (body (list-of expression?))
   (env environment?)]
  [kontinuation-proc
    (k kontinuation?)])	 
	 
	 
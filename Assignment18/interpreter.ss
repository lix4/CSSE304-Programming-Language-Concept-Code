; top-level-eval evaluates a form in the global environment

(trace-define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env (init-k))))

; eval-exp is the main component of the interpreter

(trace-define eval-exp
  (lambda (exp env k)
    (cases expression exp
      [lit-exp (datum) (apply-k k datum)]
      [var-exp (id)
		(apply-env env id k 
           (lambda () (eopl:error 'apply-env
		          "variable not found in environment: ~s"
			   id)))]
      [def-exp (dest val)
		(eval-exp val env (def-k dest k))]
			[else-exp (test then after)
        (eval-exp test 
        		  env 
        		  (else-k then after env k))]
			[if-exp (test then)
        		(eval-exp test 
        		  env 
        		  (if-k then env k))]
      [lambda-exp (parameter body)
			(apply-k k (closure parameter body env))]
			[set!-exp (dest body) 
      		(eval-exp body env (set!-k env dest k))]
					
      [quote-exp (content) (apply-k k content)]			
			
      [app-exp (rator rands)
			(eval-exp rator
					  env
					  (rator-k rands env k))]      
			[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
(define eval-bodies
  (lambda (body env k)
    (eval-exp (car body) env (body-k (cdr body) env k))))


(define map-cps
	(lambda (proc ls k)
		(if (null? ls)
			(apply-k k '())
			(proc (car ls) (map-cdr-k proc (cdr ls) k)))))

			
(define eval-rands
  (lambda (rand env k)
    (map-cps (lambda (x k) (eval-exp x env k)) 
    		 rand
    		 k)))
(define map-para
  (lambda (para args)
  	(cond ((list? para) (list para args))
  		  ((symbol? para) (list (list para) (list args)))
  		  (else (let ((result (map-para (cdr para) (cdr args))))
  		  				(list (cons (car para) (car result))
  		  					  (cons (car args) (cadr result))))))))

					

										
(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [prim-proc (op) 
		(apply-prim-proc op args k)]
      [closure (parameter body env)
      	(let* ((mapped (map-para parameter args))
      		   (para (car mapped))
      		   (vals (cadr mapped)))
			  (eval-bodies body (extend-env para vals env) k))]
      [kontinuation-proc (k1)  (apply-k k1 (car args))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(exit-list call/cc + - * / add1 sub1 zero? not = < > <= >= apply map 
			    cons list null? eq? eqv? equal? member
			    length list->vector list? pair? procedure? vector->list  append
			    vector make-vector vector-ref vector? number?
			    symbol? set-car! set-cdr! vector-set! display newline
			   car cdr caar cadr cdar cddr caaar caadr cadar cdaar caddr cdadr
			    cddar cdddr  list-tail void ))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     (list-copy *prim-proc-names*)   ;  a value (not an expression) with an identifier.
     (map prim-proc *prim-proc-names*)
     (empty-env)))
		 
(define reset-global-env
  (lambda ()
    (set! init-env
			(extend-env (list-copy *prim-proc-names*)  (map prim-proc *prim-proc-names*)  (empty-env)))))



; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.


(define apply-prim-proc
  (lambda (prim-proc args k)
    (case prim-proc
    	[(call/cc) (apply-proc (car args)
			       (list (kontinuation-proc k))
				k)]
	[(void) (apply-k k (void))]
	[(+) (apply-k k (apply + args))]
	[(-) (apply-k k (apply - args))]

	[(*) (apply-k k (apply * args))]
	[(/) (apply-k k (apply / args))]

	[(=) (apply-k k (apply = args))]
	[(<) (apply-k k (apply < args))]
	[(>) (apply-k k (apply > args))]

	[(<=) (apply-k k (apply <= args))]
	[(>=) (apply-k k (apply >= args))]
	[(apply) (apply-proc (car args) (cadr args) k)]
	[(map) (let ((proc (car args))
		     	 (arg-tuple (apply map list (cdr args))))
		    	 (map-cps (lambda (x k) (apply-proc proc x k)) arg-tuple k))]
	[(cons) (apply-k k (apply cons args))]
	[(add1) (apply-k k (apply (lambda (x) (+ x 1)) args))]
	[(sub1) (apply-k k (apply (lambda (x) (- x 1)) args))]
	[(zero?) (apply-k k (apply zero? args))]
	[(not) (apply-k k (apply not args))]
	[(list) (apply-k k (apply list args))]
	[(null?) (apply-k k (apply null? args))]
	[(eq?) (apply-k k (apply eq? args))]
		[(eqv?) (apply-k k (apply eqv? args))]
	[(equal?) (apply-k k (apply equal? args))]
  [(member) (apply-k k (apply member args))]
	[(length) (apply-k k (apply length args))]
	[(pair?) (apply-k k (apply pair? args))]
	[(list?) (apply-k k (apply list? args))]

	[(append) (apply-k k (apply append args))]
	[(list-tail) (apply-k k (apply list-tail args))]
	[(procedure?) (apply-k k (apply proc-val? args))]
	[(vector) (apply-k k (apply vector args))]
	[(vector?) (apply-k k (apply vector? args))]
	[(number?) (apply-k k (apply number? args))]
	[(symbol?) (apply-k k (apply symbol? args))]
	[(display) (apply-k k (apply display args))]
	[(newline) (apply-k k (apply newline args))]
	[(car) (apply-k k (apply car args))]
	[(cdr) (apply-k k (apply cdr args))]
	[(caar) (apply-k k (apply caar args))]
	[(cadr) (apply-k k (apply cadr args))]
	[(cdar) (apply-k k (apply cdar args))]
	[(cddr) (apply-k k (apply cddr args))]
	[(caaar) (apply-k k (apply caaar args))]
	[(caadr) (apply-k k (apply caadr args))]
	[(cadar) (apply-k k (apply cadar args))]
	[(cdaar) (apply-k k (apply cdaar args))]
	[(caddr) (apply-k k (apply caddr args))]
	[(cdadr) (apply-k k (apply cdadr args))]
	[(cddar) (apply-k k (apply cddar args))]
	[(cdddr) (apply-k k (apply cdddr args))]

	[(set-car!) (apply-k k (apply set-car! args))]
	[(set-cdr!) (apply-k k (apply set-cdr! args))]
	[(vector-set!) (apply-k k (apply vector-set! args))]
	[(vector-ref) (apply-k k (apply vector-ref args))]
	[(make-vector) (apply-k k (apply make-vector args))]
	[(vector->list) (apply-k k (apply vector->list args))]
	[(list->vector) (apply-k k (apply list->vector args))]
	[(exit-list) (apply-k (init-k) args)]
	)))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ; Answers that should display differently: procedure
      (if (not (eq? answer (void)))
	  (eopl:pretty-print (if (proc-val? answer) '<procedure> answer)))
      (rep))))  ; tail-recursive, so stack doesn't grow.




(define apply-k
  	(lambda (k v)
  		(cases kontinuation k
  			[init-k () v]
  			[else-k (then-exp else-exp env k)
  				(if v
  					(eval-exp then-exp env k)
  					(eval-exp else-exp env k))]
  			[if-k (then-exp env k)
  				(if v
  					(eval-exp then-exp env k))]				
  			[set!-k (env dest k1)
  				(begin (set!-env dest v env)
  					   (apply-k k1 (void)))]
  			[def-k (dest k)
  				(begin (extend!-env (list dest) (list v) init-env)
  					   (set!-env dest v init-env)
  					   (apply-k k (void)))]
  			[rator-k (rands env k)
				(eval-rands rands
							env
							(rands-k v k))]
			[rands-k (proc-value k)
					 (apply-proc proc-value v k)]
			[body-k (end env end-k)
					(if (null? end)
						(apply-k end-k v)
						(eval-exp (car end) env (body-k (cdr end) env end-k)))]
			[map-cdr-k (proc rest k)
				(map-cps
					proc
					rest
					(map-car-k v k))] ;v result of car
			[map-car-k (head k)
				(apply-k k (cons head v))]
  			[else (exit)]
  			)))



(trace-define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))







(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (sym val env)
    (extended-env-record sym val env)))

(define list-find-position
  (lambda (sym lis)
		(begin
		(set! tag1 1)
    (list-index (lambda (xsym) (eqv? sym xsym)) (car (list lis))))))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))
		 
(define list-set!
  (lambda (pos val lis)
    (if (= pos 0)
        (set-car! lis val)
        (list-set! (- pos 1) val (cdr lis)))))		 
(define set!-env
  (lambda (sym val env)
    (cases environment env
      (empty-env-record () (void))
      (extended-env-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
             (if pos
                 (list-set! pos val vals)
								 (set!-env sym val env)))))))

		 
(define tail
  (lambda (lst)
    (if (null? (cdr lst)) lst (tail (cdr lst)))))

(define extend!-env
  (lambda (sym val env)
    (cases environment env
      (empty-env-record () (void))
      (extended-env-record (syms vals small_env)
        (let ((pos (list-find-position sym syms)))
	           (if (not pos)
                 (let ((syms-tail (tail syms))
                       (vals-tail (tail vals)))
                       (set-cdr! syms-tail sym)
                       (set-cdr! vals-tail val))))))))
 (define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (begin
		(cases environment env
      (empty-env-record ()
        (fail))
      (extended-env-record (syms vals env)
	(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (apply-k succeed (list-ref vals pos))
	      (apply-env env sym succeed fail))))))))


(define get-address
  (lambda (sym env)
    (cases environment env
      (empty-env-record () '())
      (extended-env-record (syms vals env1)
	 (let ((pos (list-find-position sym syms)))
              (if pos
                  (list 0 pos env)
		  (let ((tail (get-address sym env1)))
		       (if (null? tail)
			   '()
			   (list (+ 1 (car tail)) (cadr tail) env)))))))))

(define set!-env-ref
  (lambda (ref val env)
    (cases environment env
      (empty-env-record () (void))
      (extended-env-record (syms vals env)
        (if (zero? (car ref))
	    (list-set! (cadr ref) val vals)
	    (set!-env-ref (list (- (car ref) 1) (cadr ref))
			  val
			  env))))))



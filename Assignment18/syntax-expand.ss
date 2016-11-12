
(define syntax-list '(let or and begin cond let* letrec case while))

(define syntax?
  (lambda (sym)
    (memq sym syntax-list)))

(define syntax-expand
  (lambda (x)
	(begin
    (cond
			((eq? 'let (car x)) (expand-let (cdr x)))
			((eq? 'let* (car x)) (expand-let* (cdr x)))
			((eq? 'letrec (car x)) (expand-letrec (cdr x)))
			((eq? 'cond (car x)) (expand-cond (cdr x)))
			((eq? 'while (car x)) (expand-while (cdr x)))
			((eq? 'and (car x)) (expand-and (cdr x)))
			((eq? 'or (car x)) (expand-or (cdr x)))
			((eq? 'begin (car x)) (expand-begin (cdr x)))
			((eq? 'case (car x)) (expand-case (cdr x)))
			))))

(define expand-let
  (lambda (expression)
		(begin
    (if (list? (car expression))
	(let ((parameter (map car (car expression)))
	      (val (map cadr (car expression)))
	      (body (cdr expression)))
	     (cons (apply list 'lambda parameter body) val))
	(let ((func (car expression))
	      (parameter (map car (cadr expression)))
	      (val (map cadr (cadr expression)))
	      (body (cddr expression)))
	     (list 'letrec 
		   (list (list func (apply list 'lambda parameter body)))
		   (cons func val)))))))
         
    
(define expand-while
  (lambda (expression)
    (let* ((test (car expression))
           (then (cdr expression))
           (loop-body (list 'lambda '() (list 'if test (append (cons 'begin then) '((loop)))))))
	  (list 'letrec (list (list 'loop loop-body)) '(loop)))))

(define expand-letrec
  (lambda (body)
		(begin
    (let* ((bind (car body))
           (tail (cdr body))
           (blind-bind (map (lambda (n) (list (car n) '())) bind))
           (reset (map (lambda (n) (cons 'set! n)) bind)))
          (cons 'let (cons blind-bind (append reset tail)))))))

(define expand-or
  (lambda (body)
    (if (null? body)
        #f
    	(let ((head (car body))
              (tail (cdr body)))
             (list 'if head head (cons 'or tail))))))

(define expand-and
  (lambda (body)
    (if (null? body)
        #t
    	(let ((head (car body))
              (tail (cdr body)))
             (list 'if head (cons 'and tail) head)))))

(define expand-begin
  (lambda (body) (append '(let ()) body)))

(define expand-cond
  (lambda (body)
		(begin
		(if (not(null? body))
    (let ((term (car body)))
         (if (eq? 'else (car term))
             (cadr term)
             (list 'if (car term)
		       (cadr term)
		       (expand-cond (cdr body)))))))))

(define expand-let*
  (lambda (body)
		(begin
    (letrec ((stack-let
               (lambda (term tail)
                 (if (null? (cdr term))
                     (apply list 'let term tail)
                     (list 'let 
                           (list (car term))
                           (stack-let (cdr term) tail))))))
            (stack-let (car body) (cdr body))))))


(define expand-case
  (lambda (body)
    (let ((head (car body))
          (then (cdr body)))
         (cons 'cond (map (lambda (x)
                             (if (eq? 'else (car x))
                    	         x
                    		 (list (list 'member head (list 'quote (car x))) 
				       (cadr x))))
               		  then)))))

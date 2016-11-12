; Paste these expressions into Scheme one at a time:


; ------------------------------------------------------------
; From http://c2.com/cgi/wiki?CallWithCurrentContinuation

(call/cc
    (lambda (return)
      (begin
        (display "One ")
        (return #t)
        (display "Two ")
        #f)))

 (define-syntax let-cc
    (syntax-rules ()
      ((let-cc variable . body)
       (call-with-current-continuation
         (lambda (variable) . body)))))

; Using this operator the example would read:
  (let-cc return
    (display "One ")
    (return #t)
    (display "Two ")
    #f)

  (call-with-current-continuation
    (lambda (exit)
      (for-each (lambda (x)
             (if (eq? x 'nuclear)
               (exit x)
               (symbol->string x)))
         '(this is a nuclear bomb in your garage))))

;--------------------------------------------------------------------------
; From https://www.gnu.org/software/guile/manual/html_node/Continuations.html

(define kont #f)
(format #t "the return is ~a\n"
        (call/cc (lambda (k)
                   (set! kont k)
                   1)))

(kont 2)

 ; C programmers may note that call/cc is like setjmp in the way it records at runtime a point in program execution. A call to a continuation is like a longjmp in that it abandons the present location and goes to the recorded one. Like longjmp, the value passed to the continuation is the value returned by call/cc on resuming there. However longjmp can only go up the program stack, but the continuation mechanism can go anywhere.

;------------------------------------------------------------------------------
; from http://rigaux.org/language-study/various/callcc/scheme.html



; if your scheme doesn't have call/cc as a short-name for call-with-current-continuation, use:
; (define-macro (call/cc f) `(call-with-current-continuation ,f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; dumb examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(call/cc (lambda (k) (k "foo")))	; => "foo"

(call/cc (lambda (k) "foo"))		; => "foo"
					; same as above since
					; (call/cc (lambda (k) expr))  <=>  (call/cc (lambda (k) (k expr)))

(call/cc (lambda (k) 
	   (k "foo")
	   (error "ignored")))		; => "foo"
					; everything after the call to "k" is ignored

(string-append
 "foo "
 (call/cc (lambda (k) "bar "))
 "boo")					; => "foo bar boo"


(define saved #f)
(string-append
 "foo "
 (call/cc (lambda (k) 
	    (set! saved k)
	    "bar "))
 "boo")					; => "foo bar boo"
(saved "BAR ")				; => "foo BAR boo"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; imperative constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; "return"
(define (inv v)
  (call/cc (lambda (return)
    (display "doing things")
    (if (= v 0) (return 0)) ; special case for v = 0
    (display "otherwise doing other things")
    (/ 1 v))))

; "goto"
(begin
  (display "doing things")
  (define label-here #f)
  ; creating a label here
  (call/cc (lambda (k) (set! label-here k)))
  (display "doing other things")
  (label-here "unused argument") ; the argument is unused since the return value of the call/cc above is dropped
  (display "this won't be reached")
  )

; "goto" v.2
(define (goto continuation) (continuation continuation))

(begin
  (display "doing things")
  (define label-here (call/cc (lambda (k) k)))
  (display "doing other things")
  (goto label-here)
  (display "this won't be reached")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; returning a special value (dropping the stack of computations)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; return the first i where (list-ref l i) = e
(define (listindex e l)
  (call/cc (lambda (not_found) ; using not_found for getting out of listindex
			       ; without computing the +1's
    (letrec ((loop 
	      (lambda (l)
		(cond
		 ((null? l) (not_found #f))
		 ((equal? e (car l)) 0)
		 (else (+ 1 (loop (cdr l))))))))
      (loop l)))))

; the same written with the special "let" construct
(define (listindex e l)
  (call/cc (lambda (not_found) ; using "not_found" for getting out of listindex
			       ; without computing the +1's
    (let loop ((l l))
      (cond
       ((null? l) (not_found #f))
       ((equal? e (car l)) 0)
       (else (+ 1 (loop (cdr l)))))))))

(define (product li)
  (call/cc (lambda (break)
     (let loop ((l li))
       (cond
	((null? l) 1)
	((= (car l) 0) (break 0)) ; using "break" as an optimization to drop the computation
	(else (* (car l) (loop (cdr l)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; "delay"ing and coroutines (inspired by http://okmij.org/ftp/Scheme/enumerators-callcc.html)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; first here is an imperative generator (a dumb one)
(define (generate use-it)
  (let loop ((i 0))
    (if (< i 10) (begin (use-it i) (loop (+ i 1))))))

; we want to use it functionnally
; for this, we generate the list out of the generator
(define (generator->list generator)
  (let ((l '()))
    (generator
     (lambda (e) 
       (set! l (cons e l))))
    (reverse l)))

; now, we want to create the list lazily, on demand.
; the generator->list above can't do this

; here is another version of generator->list that uses a coroutine to create the result
(define (generator->list generator)
  (call/cc (lambda (k-main)
    (generator 
     (lambda (e)
       (call/cc (lambda (k-reenter)
         (k-main (cons e (call/cc (lambda (k-new-main)
				    (set! k-main k-new-main)
				    (k-reenter #f)))))))))
    (k-main '())
    )))

; the advantage of the call/cc version above is that it's easy to generate the list lazily
(define (generator->lazy-list generator)
  (delay
    (call/cc (lambda (k-main)
      (generator 
       (lambda (e)
         (call/cc (lambda (k-reenter)
           (k-main (cons e 
			 (delay 
			   (call/cc (lambda (k-new-main)
				      (set! k-main k-new-main)
				      (k-reenter #f))))))))))
      (k-main '())
    ))))

; testing it:
(define (fnull? x) (null? (force x)))

(define (fcar x) (car (force x)))
(define (fcdr x) (cdr (force x)))

(define (lazy-list->list lz)
  (if (fnull? lz) '()
      (cons (fcar lz) (lazy-list->list (fcdr lz)))))

(lazy-list->list (generator->lazy-list generate))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; weird examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
((call/cc ; <= there is an apply in the saved context
   (lambda (goto)
     (letrec ((start (lambda () (display "start ") (goto next)))
	      (next  (lambda () (display "next ")  (goto last)))
	      (last  (lambda () (display "last")  "done")))
       start))))	       ; => returns "done", displays "start next last"
; to ease understanding, try:
((lambda () (display "start ")))

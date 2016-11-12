; This code is from Page 539 of "Scheme and the Art of Programming"
; by Springer and Friedman.
; to use it, load this code, then type ((call/cc receiver4)).
;  Then you can use the escaper procedure from class.

(define call/cc call-with-current-continuation)

(define *escape/thunk* #f)

(define receiver-4
  (lambda (continuation)
    (set! *escape/thunk* continuation)
    (*escape/thunk* (lambda () (display "escaper is defined")))))

(define escaper
  (lambda (proc)
    (lambda args
      (*escape/thunk*
       (lambda () (apply proc args))))))

    

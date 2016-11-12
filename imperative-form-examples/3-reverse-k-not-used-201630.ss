;----- file 3-reverse-k.ss -------------------
;----- now we treat continuations as an ADT---

; For each kind of continuation, we create a
; "make" procedure.   We also create an
; apply-continuation procedure that knows how to apply
; any continuation from this code.

(define reverse*-k
  (lambda (L k)
    (if (null? L)
        (apply-continuation k '())
        (reverse*-k (cdr L)
                    (make-rev1 k L)))))

(define append-k
  (lambda (a b k)
    (if (null? a)
        (apply-continuation k b)
        (append-k (cdr a) b (make-append-cont k a)))))

; Te above code is independent of the
; representation of continuations.


; Representation #1.
; As we did with finite functions,  we first
; implement continuations as scheme procedures. 

(define make-rev1
  (lambda (k L)
    (lambda (reversed-cdr)
      (if (pair? (car L))
          (reverse*-k (car L)
                      (make-rev2
                       reversed-cdr k))
          (append-k reversed-cdr
                    (list (car L)) k)))))

(define make-rev2
  (lambda  reversed-cdr k)
    (lambda (reversed-car)
      (append-k reversed-cdr
                (list reversed-car) k))))

(define make-init-k
  (lambda ()
    (lambda (v)
      (display "answer: ")
      (display v)
      (newline))))

(define make-append-cont
  (lambda (k a)
    (lambda (appended-cdr)
      (apply-continuation
       k
       (cons (car a) appended-cdr)))))

(define apply-continuation
  (lambda (k v) (k v)))

(define testk
  (lambda () 
    (reverse*-k
     '(1 ((2 3) () (((4)))))
     (make-init-k))))







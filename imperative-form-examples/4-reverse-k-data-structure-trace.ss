;--- file 4-reverse-k-data-structure.ss -------------------
;--- implement the continuations ADT as data-structures ---
;--- use the same definitions of reverse-k and append-k ---
;--- as in 3-reverse-k.ss ---------------------------------

(load "record.ss")
(define-record append-k (k a))
(define-record init-k ())
(define-record rev1-k (k L))
(define-record rev2-k (reversed-cdr k))

(trace-define apply-continuation
  (lambda (k v)
    (variant-case k
      [init-k ()
           (display "answer: ")
           (display v)
           (newline)]
      [append-k (k a)
        (apply-continuation k
                            (cons (car a)
                                  v))]
      [rev1-k (k L)
        (if (pair? (car L))
            (reverse*-k (car L)
                        (make-rev2-k v k))
            (append-k v
                      (list (car L))
                      k))]
      [rev2-k (reversed-cdr k)
              (append-k
               reversed-cdr
               (list v)
               k)])))

(trace-define reverse*-k
  (lambda (L k)
    (if (null? L)
        (apply-continuation k '())
        (reverse*-k (cdr L)
                    (make-rev1-k k L)))))

(trace-define append-k
  (lambda (a b k)
    (if (null? a)
        (apply-continuation k b)
        (append-k (cdr a)
                  b
                  (make-append-k k a)))))

(define testk
  (lambda () 
    (reverse*-k '(1 ((2 3) () (((4)))))
                (make-init-k))))


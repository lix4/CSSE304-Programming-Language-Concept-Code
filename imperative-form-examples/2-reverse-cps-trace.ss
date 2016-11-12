;------------ file 2-cps.ss ------------------
;------------ a cps version of reverse. -----

(define apply-k (lambda (k v) (k v)))

(trace-define reverse*-cps
  (lambda (L k)
    (if (null? L)
        (apply-k k '())
        (reverse*-cps
         (cdr L)
         (trace-lambda cdr-k (reversed-cdr)
           (if (pair? (car L))
               (reverse*-cps
                (car L)
                (trace-lambda car-k (reversed-car)
                  (append-cps reversed-cdr
                              (list reversed-car)
                              k)))
               (append-cps reversed-cdr
                           (list (car L))
                           k)))))))

(trace-define append-cps
  (lambda (a b k)
    (if (null? a)
        (apply-k k b)
        (append-cps
         (cdr a)
         b
         (trace-lambda append-k (appended-cdr)
           (apply-k k (cons (car a) appended-cdr)))))))

(trace-define init-k
  (lambda (v)
    (display "answer: ") (display v) (newline)))

; (trace reverse*-cps append-cps init-k apply-k)





;------------ file 2-cps.ss ------------------
;------------ a cps version of reverse. -----

(define apply-k (lambda (k v) (k v)))

(define reverse*-cps
  (lambda (L k)
    (if (null? L)
        (apply-k k '())
        (reverse*-cps
         (cdr L)
         (lambda (reversed-cdr)
           (if (pair? (car L))
               (reverse*-cps
                (car L)
                (lambda (reversed-car)
                  (append-cps reversed-cdr
                              (list reversed-car)
                              k)))
               (append-cps reversed-cdr
                           (list (car L))
                           k)))))))

(define append-cps
  (lambda (a b k)
    (if (null? a)
        (apply-k k b)
        (append-cps
         (cdr a)
         b
         (lambda (appended-cdr)
           (apply-k k (cons (car a) appended-cdr)))))))

(define init-k
  (lambda (v)
    (display "answer: ") (display v) (newline)))





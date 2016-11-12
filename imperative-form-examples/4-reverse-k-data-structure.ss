;--- file 4-reverse-k-data-structure.ss -------------------
;--- implement the continuations ADT as data-structures ---
;--- use the same definitions of reverse-k and append-k ---
;--- as in 3-reverse-k.ss ---------------------------------

(load "chez-init.ss")
(define any? (lambda (x) #t))

(define-datatype continuation continuation?
  [append-k
   (a any?)
   (k continuation?)]
  [init-k]
  [rev1-k
   (L any?)
   (k continuation?)]
  [rev2-k
   (reversed-cdr (list-of any?))
   (k continuation?)])

(define reverse*-cps
  (lambda (L k)
    (if (null? L)
        (apply-k k '())
        (reverse*-cps (cdr L)
                  (rev1-k L k)))))

(define test
  (lambda () 
;   (reverse*-cps '(a ((b c) () (((d)))))
    (reverse*-cps '(() b)
                (init-k))))

(define append-cps
  (lambda (a b k)
    (if (null? a)
        (apply-k k b)
        (append-cps (cdr a)
                  b
                  (append-k a k)))))

(define apply-k
  (lambda (k v)
    (cases  continuation k
      [init-k ()
           (printf "answer: ~s~n" v)]
      [append-k (a k)
        (apply-k k
		 (cons (car a) v))]
      [rev1-k (L k)
        (if (pair? (car L))
            (reverse*-cps (car L) (rev2-k v k))
            (append-cps v (list (car L)) k))]
      [rev2-k (reversed-cdr k)
              (append-cps reversed-cdr (list v) k)])))

(define trace-all
  (lambda ()
    (trace test reverse*-cps append-cps apply-k init-k rev1-k rev2-k append-k)))

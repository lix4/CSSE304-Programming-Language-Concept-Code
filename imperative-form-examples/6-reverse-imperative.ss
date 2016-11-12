; -- file 6-reverse-imperative.ss ----------------------
; -- imperative version of  reverse* --------------------
; -- This puts it into a form where all of the ---------
; -- recursion can be replaced by assignments and "goto"s

(load "chez-init.ss")
(define any? (lambda (x) #t))

(define-datatype continuation continuation?
  [append-k
   (k continuation?)
   (a any?)]
  [init-k]
  [rev1-k
   (k continuation?)
   (L any?)]
  [rev2-k
   (reversed-cdr (list-of any?))
   (k continuation?)])


(define reverse*
  (lambda (L)
    (let ([L L]
          [k (init-k)]
          [a '*unbound]
          [b '*unbound]
          [v '*unbound*])
      (letrec
          ([reverse*
              (lambda ()
                (if (null? L)
                    (begin
                      (set! v '())
                      (apply-k))
                    (begin 
                      (set! k (rev1-k k (car L)))
                    (set! L (cdr L))
                    (reverse*))))]
           [append
               (lambda ()
                 (if (null? a)
                     (begin
                       (set! v b)
                       (apply-k))
                     (begin
                       (set! k (append-k k a))
                       (set! a (cdr a))
                       (append))))]
           [apply-k
                (lambda ()  
                  (cases continuation k
                    [init-k ()
                      (printf "answer: ~s~n " v)]
                    [append-k (k1 a)
                       (set! v (cons (car a) v))
                       (set! k k1)
                       (apply-k)]
                    [rev1-k (k1 car-L)
                       (if (pair? car-L)
                           (begin
                             (set! L car-L)
                             (set! k (rev2-k v k1))
                             (reverse*))
                           (begin
                             (set! a v)
                             (set! b (list car-L))
                             (set! k k1)
                             (append)))]
                    [rev2-k (reversed-cdr k1)
                       (set! a reversed-cdr)
                       (set! b (list v))
                       (set! k k1)
                       (append)]))])
        (reverse*)))))


(define testk
    (lambda ()
          (reverse*
           '(1 ((2 3) () (((4))))))))




; --file 1-reverse.ss ----------------------------
; --Starting point: original version of reverse* --

(define reverse* ; reverse* is essentially slist-reverse
  (lambda (L)
    (if (null? L)
        '()
        (append (reverse* (cdr L))
                (list (if (pair? (car L))
                          (reverse* (car L))
                          (car L)))))))
;(define append     ; this is a comment 
;  (lambda (a b)    ; because append is already 
;    (if (null? a)  ; defined in scheme
;       b
;       (cons (car a)
;             (append (cdr a) b)))))




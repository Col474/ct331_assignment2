#lang racket

(provide ins_beg)
(provide ins_end)
(provide count_top_level)
(provide count_instances)
(provide count_instances_tr)
(provide count_instances_deep)
 
(define (ins_beg el lst)
 (cons el lst))

(define (ins_end el lst)
  (append lst (list el)))

(define (count_top_level lst)
(length lst)
)


(define (count_instances el lst)
    (cond
      ((null? lst)     0)
      ((eq? el (car lst)) (+ 1 (count_instances el (cdr lst))))
      (else          (count_instances el (cdr lst)))))
  
(define (count_instances_tr el lst)
  (let counting ((lst lst) (n 0))
    (if (null? lst)
        n
        (counting (cdr lst)
                  (if (eq? el (car lst))
                      (+ n 1)
                      n)))))

(define (count_instances_deep el lst)
      (cond
        ( (if (eqv? el lst) 1 0))
        (else (+
                (count_instances_deep el (car lst))
                (count_instances_deep el (cdr lst))
                ))
        )
    )


   
>(ins_beg 'a '(b c d))
>(ins_beg '(a b) '(b c d))

>(ins_end 'a '(b c d))
>(ins_end '(a b) '(b c d))

>(count_top_level'(b c d (a b)) )
>(count_instances 'a '(a b c d a))
>(count_instances_tr 'a '(a b c d a))
>(count_instances_deep 'a '(a b c d a(a b c)))
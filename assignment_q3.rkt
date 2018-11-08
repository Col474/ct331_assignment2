#lang racket

(provide display_tree)
(provide search_tree)
(provide insert)
(provide insert_list)
(provide sort)


(define (display_tree tree)
  (cond((null tree))
       (1(display_tree(car tree))
         (print(cadr tree)
               (display_tree(caddr))))))


(define(search_tree el list)
(member el (list))
  )


(define (insert el list)
  (cond
    [(empty? list) 
     (list empty el list)]
    [(< el (cadr list))
     (list (insert (car list) el) (cadr list) (caddr list))] 
    [(> el (cadr list)) 
     (list (car list) (cadr list) (insert (caddr list) el))] 
    [else 
     list] 
    )
  )

(define (insert_list list1 list2)
  (cond
    [(null? list1) list2]
    [(null? list2) list1]
    [(< (car list1) (car list2))
     (cons (car list1) (insert_list (cdr list1) list2))]
     (cons (car list2) (insert_list list (cdr list1)))))

(define (sort tree)
  (cond
    [(or (null? tree) (null? (cdr tree))) tree]
    [(null? (cddr tree))
     (insert_list (list (car tree)) (cdr tree))]
    [#t
     (let ([x (ceiling (/ (length tree) 2))])
       (insert_list (sort (take tree x))
                    (sort (drop tree x))))]))

>(display_tree '(1 2 3 4 5 6))
>(search_tree '4 '(1 2 3 4 5))
>(insert '3 '(1 2 4 5 6))
>(insert_list '(2 4 6 8) '(1 3 5 7))
>(sort '(3 6 2 1 8))
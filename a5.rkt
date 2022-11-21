; Right reduce
; (fn a (fn b (fn c i)))
(define (right-reduce fn list init)
  (if (null? list) init
      (fn (car list)
          (right-reduce fn (cdr list) init))))

; Left reduce
; ((((i • a) • b) • c)
(define (left-reduce fn init list)
  (if (null? list) init
      (left-reduce fn (fn init (car list)) (cdr list))))

; gets the first item for each in the list
(define firstOfList (lambda (lst) (map car lst)))

; checks if an element is a member in the lst
(define member? (lambda (x lst)
                 (if (null? lst)
                     #f
                     (if (eq? x (car lst))
                         #t
                         (member x (cdr lst))
                     )
                 )
            ))

; gets the second element in a list
(define second (lambda (lst) (car (cdr lst))))







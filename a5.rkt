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

; gets third element in list
(define third (lambda (lst)
                     (car (cdr (cdr lst)))
                     ))

; checks if its a symbol and not a reserved symbol
(define isProperSymbol? (lambda (elem)
                         (and (symbol? elem) (not (eq? elem 'lmda)))
                         ))

; checks if the current element is an E
(define isE? (lambda (elem)

               (cond
                 ((isProperSymbol? elem) #t)
                 ((isLmda? elem) #t)
                 ((and (not (symbol? elem)) (and (> (length elem) 1) (and (isE? (car elem)) (isE? (second elem))))) #t)
                 (else #f)
                 )

               ))

; checks if the current element is an e
(define isLmda? (lambda (elem)
                  (and
                  (and
                   (and
                    (not (symbol? elem))
                   (eq? (car elem) 'lmda))
                   (and (> (length elem) 1) (isProperSymbol? (second elem)))
                   )
                  (and (> (length elem) 2) (isE? (third elem))))
                  ))

(isE? '(lmda x (x x)))

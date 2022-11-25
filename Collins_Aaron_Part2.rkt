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

; checks if the current element is an application
(define isApp? (lambda (elem)
                (if
                 (and (not (symbol? elem)) (and (= (length elem) 2) (and (isE? (car elem)) (isE? (second elem)))))
                 #t
                 #f)
                ))

; checks if the current element is a list of length 2 (useful for executing imperfect expressions)
(define isArrLength2? (lambda (elem) (and (not (symbol? elem)) (= (length elem) 2))))

; checks if the current element is an E
(define isE? (lambda (elem)

               (cond
                 ((isProperSymbol? elem) #t)
                 ((isLmda? elem) #t)
                 ((isApp? elem) #t)
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
                   (and (= (length elem) 3) (isProperSymbol? (second elem)))
                   )
                  (and (= (length elem) 3) (isE? (third elem))))
                  ))

; Substitutes the proper expressions recursively
(define recSubst (lambda (expr replaceWith replaceExpr boundList)
             (if (member? replaceExpr boundList)
                expr
                (cond
                  
                  ((and
                    (isProperSymbol? expr)
                    (eq? expr replaceExpr)
                    )
                   replaceWith
                   )

                  (
                   (isArrLength2? expr)
                   (cons (recSubst (car expr) replaceWith replaceExpr boundList) (cons (recSubst (second expr) replaceWith replaceExpr boundList) '()))
                   )
                  
                   ((and
                    (isLmda? expr)
                    (not (eq? (second expr) replaceExpr))
                    )
                   (cons (car expr) (cons (second expr) (cons (recSubst (third expr) replaceWith replaceExpr (cons (second expr) boundList)) '())))
                   )
                   
                  (else expr)
                  )
                ))
             )

; Substitutes the proper expressions
(define subst (lambda (expr replaceWith replaceExpr) (recSubst expr replaceWith replaceExpr '())))


; Used for beta-reduction
(define beta-reduce (lambda (expr)

                      (if (and (isApp? expr) (isLmda? (car expr)))
                          
                          (beta-reduce (subst (third (car expr)) (second expr) (second (car expr))))
                          
                          expr
                      )
                      
                      ))


(display "########## Beta-Reduce Results ##################\n")
(beta-reduce 'x)
;x
(beta-reduce '(x x))
;(x x)
(beta-reduce '(lmda x x))
;(lmda x x)
(beta-reduce '((lmda x x) e))
;e
(beta-reduce '((lmda x x) ((lmda y y) z)))
;z
(beta-reduce '((lmda x y) ((lmda x (x x)) (lmda x (x x)))))
;y
(display "########## END OF Beta-Reduce Results ##################\n")

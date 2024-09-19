(define sqr
  (lambda (x)
    (* x x)))

(define sqr-list
  (lambda (lst)
       (cond
         [(null? lst) '()]
         [else (cons (sqr (car lst)) (sqr-list (cdr lst)))]
         )
       )
     )

(define change
  (lambda (x)
    (cond
      [(< x 0) (sqr x)]
      [(> x 0) (+ x 1)]
      [(= x 0) 0]
    )
  )
)

(define change-list
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [else (cons (change (car lst)) (change-list (cdr lst)))]
      )
    )
  )

(define dist_pt
  (lambda (pt1 pt2)
  	(sqrt (+ (sqr(- (car pt1) (car pt2))) (sqr(- (cadr pt1) (cadr pt2)))))
    )
  )


(define closest-point 
  (lambda (point lst)
    (let f ([min '(-1 -1)] [ls lst]) ;If the list is empty is should return (-1 -1)
      (cond
        [(null? ls) min]
        [else
          	[cond
              	[(equal? '(-1 -1) min) (f (car ls) (cdr ls))]
               [(< (dist_pt point (car ls)) (dist_pt point min)) (f (car ls) (cdr ls))]
               [else (f min (cdr ls))]]
          ]
        )
      )
    )
  )

(define place
  (lambda (x lst)
    (if (null? lst)
      (list x)
    ;else
      (place2 x (car lst) (cdr lst) 1))))  

(define place2 ;I think I cooked
  (lambda (x carlst cdrlst check) ;check is to check if we have already placed x
    (cond
      [(null? cdrlst) 
       (if (>= x carlst)
           (list carlst x)
       ;else
           (list carlst))]
      [(and (= check 1) (> x carlst)) (cons carlst (place2 x (car cdrlst) (cdr cdrlst) check))]
      [(and (= check 1) (= x carlst)) (cons x (place2 x carlst cdrlst -1))]
      [(and (= check 1) (< x carlst)) (cons x (place2 x carlst cdrlst -1))]
      [else (cons carlst (place2 x (car cdrlst) (cdr cdrlst) -1))]
      )
    )
  )

(define delete-lists
    (lambda (lst)
      (cond
        [(null? lst) '()]
        [(list? (car lst)) (delete-lists (cdr lst))]
        [else (cons (car lst) (delete-lists (cdr lst)))]
       )
     )
  )

(define add-list
  (lambda (lst1 lst2)
    (cond
      [(null? lst1) '()]
      [else (cons (+ (car lst1) (car lst2)) (add-list (cdr lst1) (cdr lst2)))]
       )
    )
  )

(define flatten
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [(list? (car lst)) (append (flatten (car lst)) (flatten (cdr lst)))]
      [else (cons (car lst) (flatten (cdr lst)))]
      )
    )
  )
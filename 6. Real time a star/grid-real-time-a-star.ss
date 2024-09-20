(define path-lst '())
(define visited 1)
(define current-node start) ;current-node must be accessible anywhere

(define expand 
  (lambda (point)
    ;(display "expand")
    (let ((lst (adjacentv point)))
     ;(display "expand list")
      ;(display lst)
      (set-lst-visited lst)
      (add-to-path-lst lst point)
      (let loop ((exhaust-lst lst))
        (if (null? exhaust-lst) '()
       ;else
       (begin (insert (car exhaust-lst)) (loop (cdr exhaust-lst))))
      ))))


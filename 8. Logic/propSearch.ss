
(define rules
  '(
  ;finish conditions 
  ;if visited node x = t and goal node x = t, then finished = t 
  ((visited00 goal00)(finished))
  ((visited01 goal01)(finished))
  ((visited02 goal02)(finished))
  ((visited10 goal10)(finished))
  ((visited20 goal20)(finished))
  ((visited11 goal11)(finished))
  ((visited12 goal12)(finished))
  ((visited21 goal21)(finished))
  ((visited22 goal22)(finished))

  ;movement conditions
  ;if legal move make it
  ((visited00 (#f obstacle01)) (visited01)) 
  ((visited00 (#f obstacle10)) (visited10))
  
  ((visited01 (#f obstacle02)) (visited02))
  ((visited01 (#f obstacle00)) (visited00))
  ((visited01 (#f obstacle11)) (visited11)) 

  ((visited02 (#f obstacle01)) (visited01))
  ((visited02 (#f obstacle12)) (visited12))  

  ((visited10 (#f obstacle11)) (visited11))  
  ((visited10 (#f obstacle20)) (visited20))
  ((visited10 (#f obstacle00)) (visited00))

  ((visited11 (#f obstacle12)) (visited12))
  ((visited11 (#f obstacle10)) (visited10))  
  ((visited11 (#f obstacle21)) (visited21))
  ((visited11 (#f obstacle01)) (visited01))

  ((visited12 (#f obstacle22)) (visited22))  
  ((visited12 (#f obstacle02)) (visited02))  
  ((visited12 (#f obstacle11)) (visited11))

  ((visited20 (#f obstacle21)) (visited21))
  ((visited20 (#f obstacle10)) (visited10))

  ((visited21 (#f obstacle22)) (visited22))  
  ((visited21 (#f obstacle20)) (visited20))
  ((visited21 (#f obstacle11)) (visited11))

  ((visited22 (#f obstacle21)) (visited21))  
  ((visited22 (#f obstacle12)) (visited12))  
  ))


(define facts
  ;default facts
  '(goal22 visited00 obstacle11 obstacle21))

(define ModusPonens ;didnt change this
  (lambda (rule)
    (ModusPonens2 (car rule) (cadr rule))))

(define ModusPonens2 
  (lambda (b a)
    (if (null? b)  ;if we iterated through the antecedent successfully
        a  ;return the consequent
        (let ((temp (car b)))
          (if (or ;if
          (member temp facts) ;temp is a member of facts
                  ;or 
                  (and (list? temp) (equal? (car temp) #f) (not (member (cadr temp) facts)))) ;if this is a valid next move
              (ModusPonens2 (cdr b) a)  ;go to the next guy
              ;else
              '()  ;get out of here
)))))
          
(define search 
  (lambda (count) 
    (cond
      [(member 'finished facts) ;if its a fact that we finished
       (begin
       (display "goal found") ;we found the goal
       ;(display facts)
       (newline))]
      
      [(<= count 0) ;if we ran out of iterations
       (begin
       ;(display facts)
       (display "not found")) ;we didint find the goal
       (newline)]
      
      [else ;go through the rules, making new rules with modus ponens until we run out of iterations
        (let* ((firstRule (car rules)) 
               (remainingRules (cdr rules))
               (newFact (ModusPonens firstRule)))
          (if (not (null? newFact)) ;if this new fact is valid
                (set! facts (append newFact facts)) ;add that bad boy to facts
          )
          (set! rules (append remainingRules (list firstRule))) ;put the front of rules at the back 
          (search (- count 1)) ;we used up one iteration 
          )
        ]
    )
  )
)

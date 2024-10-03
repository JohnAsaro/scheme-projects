;this doesnt work, I cant get it to work, I am sleepy, im going to comment what its supposed to do and go to bed, ill do better on the next one


(define rules
  '(
  ;finish conditions  
  ;if visited node x = t and goal node x = t, then finished = t
  ((visited00 goal00)(finished)) ((visited01 goal01)(finished)) ((visited02 goal02)(finished))
  ((visited10 goal10)(finished)) ((visited20 goal20)(finished)) ((visited11 goal11)(finished))
  ((visited12 goal12)(finished)) ((visited21 goal21)(finished)) ((visited22 goal22)(finished))

  ;movement conditions
  ;if legal move make it
  ((visited00 (not obstacle01)) (visited01)) ((visited01 (not obstacle02)) (visited02)) ((visited10 (not obstacle11)) (visited11))  
  ((visited11 (not obstacle12)) (visited12)) ((visited20 (not obstacle21)) (visited21)) ((visited21 (not obstacle22)) (visited22))  
  ((visited01 (not obstacle00)) (visited00)) ((visited02 (not obstacle01)) (visited01)) ((visited11 (not obstacle10)) (visited10))  
  ((visited12 (not obstacle11)) (visited11)) ((visited21 (not obstacle20)) (visited20)) ((visited22 (not obstacle21)) (visited21))  
  ((visited00 (not obstacle10)) (visited10)) ((visited01 (not obstacle11)) (visited11)) ((visited02 (not obstacle12)) (visited12))  
  ((visited10 (not obstacle20)) (visited20)) ((visited11 (not obstacle21)) (visited21)) ((visited12 (not obstacle22)) (visited22))  
  ((visited10 (not obstacle00)) (visited00)) ((visited11 (not obstacle01)) (visited01)) ((visited12 (not obstacle02)) (visited02))  
  ((visited20 (not obstacle10)) (visited10)) ((visited21 (not obstacle11)) (visited11)) ((visited22 (not obstacle12)) (visited12))  
  ))


(define facts
  ;default facts
  '(goal22 visited00 obstacle11 obstacle21))

(define ModusPonens ;didnt change this
  (lambda (rule)
    (ModusPonens2 (car rule) (cadr rule))))
      
(define ModusPonens2 
  (lambda (b a)
    (cond
      [(null? b) (list a)] ;if we iterated through the ancedent successfully, return the consequent 
      [(member (car b) facts) (ModusPonens2 (cdr b) a)] ;if the first part of the ancedent of the rule is a fact, iterate to the next part of the ancedent
      [else '()]))) ;if we couldnt find the fact, then its not true

(define search 
  (lambda (count) 
    (cond
      [(member 'finished facts) ;if its a fact that we finished
       (begins
       (display "goal found") ;we found the goal
       (display facts)
       (newline))]
      
      [(<= count 0) ;if we ran out of iterations
       (begin
       (display facts)
       (display "not found")) ;we didint find the goal
       (newline)]
      
      [else ;go through the rules, making new rules with modus ponens until we run out of iterations
        (let* ((firstRule (car rules)) 
               (remainingRules (append (cdr rules) (list firstRule)))
               (newFact (ModusPonens firstRule)))

          (if (not (null? newFact)) ;if this new fact is valid
              (begin
                (set! facts (append newFact facts)) ;add that bad boy to facts
                (search (- count 1)))
              ;else this new fact is not valid
              (begin
                (search (- count 1)))))])))

;idk sorry Jim im too tired to figure this one out you guys win this round ig :/ 
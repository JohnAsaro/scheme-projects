;***************************** evaluate-ind **************************
; Evaluates and returns the fitness of the input individual.  
; Returns the fitness of the ind

(define int-target-list '(8 16 32 64 128))

;Evaluate that proritizes the integers being as large as possible
(define GAevaluate-ind
  (lambda (ind)
    (if (not (null? ind)) ;uncomment this and next line, and evaluate generated population to actually see the population of nums generated
      (begin (display (car ind)) (newline)))
    (if (null? ind)
        0 ;base case
        (+ (* (car ind) (car ind)) (GAevaluate-ind (cdr ind)))))) ;An individuals fitness is its number squared, so a 4 is exponentially more fit than a 2, and so on

  
;Evaluate that proritizes having all integers be disible by 10

(define helper-ind
  (lambda (ind)
    (display ind) ;uncomment this and next line, and evaluate generated population to actually see the population of nums generated
    (newline) 
    (if (= (modulo ind 10) 0)
      15 ;if divisible by 10, output 15, heavily proritizing being exactly divisible by 10
    ;else
      (- 10 (modulo ind 10))))) ;give more and more points the closer it gets to being divisible by 10
  
  
;(define GAevaluate-ind
;  (lambda (ind)
;    (if (null? ind)
;        0 ;base case
;        (+ (helper-ind (car ind)) (GAevaluate-ind (cdr ind))))))

;Evaluate that proritizes int-target-list
;(define GAevaluate-ind
;  (lambda (ind)
;    (if (not (null? ind)) ;uncomment this and next line, and evaluate generated population to actually see the population of nums generated
;      (begin (display (car ind)) (newline)))
;    (if (null? ind)
;        0 ;base case
;        (+ (if (member (car ind) int-target-list) 10 0) (GAevaluate-ind (cdr ind)))))) ;An individuals fitness is 10 if its part of the list, 0 if not
#lang racket
;changed this to prefix

; evaluate the value of an expression
(define Mvalue
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? (operator expression) '+) (+ (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '-) (- (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '*) (* (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '/) (quotient (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '%) (remainder (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      (else (error 'badop "Bad operator")))))

; evaluate the truth of a statement
(define Mboolean
  (lambda (expression)
    (cond
      ((boolean? expression) expression)
      ((eq? (operator expression) '==) (eq? (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '!=) (not (eq? (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression)))))
      ((eq? (operator expression) '<) (< (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '>) (> (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '>=) (>= (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '<=) (<= (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '&&) (and (Mboolean (leftoperand expression)) (Mboolean (rightoperand expression))))
      ((eq? (operator expression) '||) (or (Mboolean (leftoperand expression)) (Mboolean (rightoperand expression))))
      ((eq? (operator expression) '!) (not (Mboolean (leftoperand expression))))
      (else (error 'badop "Bad operator")))))
      

; helper functions to abstract the operator and operands of binary expressions.
; here the assumption is the operator is "infix".  To change it so the operator is "prefix"
; or "postfix" only requires changing the helpers and not the main code above
(define operator
  (lambda (exp)
    (car exp)))
(define leftoperand cadr)
(define rightoperand caddr)

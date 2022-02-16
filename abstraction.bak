#lang racket
;changed this to prefix


;evaluate an expression
;(define Mexpression
;  (lambda (expression state)
 ;   (cond
  ;   ((
    


; evaluate the value of an expression
(define Mvalue
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((exists? expression state) (find expression state))
      ((eq? (operator expression) '+) (+ (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '-) (- (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '*) (* (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '/) (quotient (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      (else (Mboolean expression))))) ; might work
 
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

(define Mdeclare
  (lambda (expression state)
    (cond
      ((not (null? (cdr (cdr expression)))) (cons (list (cadr expression) (Mvalue(caddr expression))) state))
      ((not (null? (cadr expression))) (cons (list (cadr expression)) state)))))

;assuming exists and insert work properly
(define Massign
  (lambda (expression state)
    (cond
      ((if (exists? (var state))) (insert (car expression) (cdr expression) state))
      (else (error 'varnotdeclared "Var not declared")))))

; assuming we pass in expression after "return"
(define Mreturn
  (lambda (expression state)
    (cond
      (
      ((if (exists? (var state))) (find var state))
      (else (error 'invalidreturn "Invalid return"))

; implement exists, check if atom and check if present in state

; implement insert (basically what we did in class)

(define Mstate
  (lambda (expression state)
    (cond
      ((eq? (car expression) 'var) (Mdeclare expression state)))))
      
      

; helper functions to abstract the operator and operands of binary expressions.
; here the assumption is the operator is "infix".  To change it so the operator is "prefix"
; or "postfix" only requires changing the helpers and not the main code above
(define operator
  (lambda (exp)
    (car exp)))
(define leftoperand cadr)
(define rightoperand caddr)

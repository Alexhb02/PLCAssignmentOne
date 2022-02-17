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
      ((exists? expression state) (get expression state))
      ((eq? (operator expression) '+) (+ (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '-) (- (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '*) (* (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '/) (quotient (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      (else (Mboolean expression state))))) ; might work
 
; evaluate the truth of a statement
(define Mboolean
  (lambda (expression state)
    (cond
      ((boolean? expression) expression)
      ((eq? (operator expression) '==) (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '!=) (not (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression)state))))
      ((eq? (operator expression) '<) (< (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '>) (> (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '>=) (>= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '<=) (<= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '&&) (and (Mboolean (leftoperand expression) state) (Mboolean (rightoperand expression) state)))
      ((eq? (operator expression) '||) (or (Mboolean (leftoperand expression) state) (Mboolean (rightoperand expression) state)))
      ((eq? (operator expression) '!) (not (Mboolean (leftoperand expression) state)))
      (else ((error 'badop "Bad operator"))))))

(define Mdeclare
  (lambda (expression state)
    (cond
      ((not (null? (cdr (cdr expression)))) (cons (list (cadr expression) (Mvalue(caddr expression))) state))
      ((not (null? (cadr expression))) (cons (list (cadr expression)) state)))))

;assuming exists and insert work properly
(define Massign
  (lambda (expression state)
    (cond
      ((exists? (car expression) state) (insert (car expression) (Mvalue (cadr expression) state) state))
      (else (error 'varnotdeclared "Var not declared")))))

; assuming we pass in expression after "return"
(define Mreturn
  (lambda (expression state)
    (cond
      ((null? expression) (error `invalidreturn "Invalid return"))
      (else (Mvalue expression state)))))

(define Mif
  (lambda (expression state)
    (cond
      ((Mboolean (conditional expression) state) (Mstate (then_s expression) state))
      (else (Mstate (else_s expression) state)))))

(define Mwhile
  (lambda (expression state)
    (cond
      [(not (Mboolean (conditional expression) state)) state]
      [else (Mwhile (conditional expression) (then_s expression) (Mstate (then_s expression) state))]
     ))) 
     
(define then_s cadr)
(define conditional car)
(define else_s caddr)

; implement exists, check if atom and check if present in state
(define exists?
  (lambda (var state)
    (cond
      ((pair? var) #f)
      ((null? state) #f)
      ((eq? var (caar state)) #t)
      (else (exists? var (cdr state))))))

; get the value stored in the state
(define get
  (lambda (var state)
    (cond
      ((null? state) (error 'varnotdeclared "The variable has not been declared"))
      ((eq? var (caar state)) (car (cdar state)))
      (else (get var (cdr state))))))

; implement insert (basically what we did in class)
(define insert
  (lambda (var value state)
    (cond
      ((null? state) (error 'varnotdeclared "The variable has not been declared"))
      ((eq? var (caar state)) (cons (cons (caar state) (list value))(cdr state)))
      (else (cons (car state) (insert var value (cdr state)))))))

(define Mstate
  (lambda (expression state)
    (cond
      ((eq? (car expression) 'var) (Mdeclare (cdr expression) state))
      ((eq? (car expression) 'if) (Mif (cdr expression) state))
      ((eq? (car expression) '=) (Massign (cdr expression) state))
      ((eq? (car expression) 'while) (Mwhile (cdr expression) state))
      ((eq? (car expression) 'return) (Mreturn (cdr expression) state)))))
      
      

; helper functions to abstract the operator and operands of binary expressions.
; here the assumption is the operator is "infix".  To change it so the operator is "prefix"
; or "postfix" only requires changing the helpers and not the main code above
(define operator
  (lambda (exp)
    (car exp)))
(define leftoperand cadr)
(define rightoperand caddr)

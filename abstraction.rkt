#lang racket
;changed this to prefix

(require "simpleParser.rkt")

;evaluate an expression
;(define Mexpression
;  (lambda (expression state)
 ;   (cond
  ;   ((
(define interpreter
  (lambda (filename)
    (interpreter-help (parser filename) '((return)))
    ))

(define interpreter-help
  (lambda (syntaxtree state)
    (cond
      [(null? syntaxtree) (get 'return state)]
      [else (interpreter-help (cdr syntaxtree) (Mstate (car syntaxtree) state))]
      )))

; evaluate the value of an expression
(define Mvalue
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((exists? expression state) (get expression state))
      ((boolean? expression) expression)
      ((not (list? expression)) (error 'varnotdeclared "Var not declared"))
      ((eq? (operator expression) '+) (+ (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((and (eq? (operator expression) '-) (null? (cddr expression))) (- 0 (Mvalue (leftoperand expression) state) ))
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
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
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
      ((not (null? (cdr (cdr expression)))) (cons (list (cadr expression) (Mvalue (caddr expression) state)) state))
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
      (else (insert 'return (Mvalue expression state) state)))))

(define Mif
  (lambda (expression state)
    (cond
      ((Mboolean (conditional expression) state) (Mstate (then_s expression) state))
      ((not (null? (cddr expression))) (Mstate (else_s expression) state))
      (else state))))

(define Mwhile
  (lambda (expression state)
    (cond
      [(not (Mboolean (conditional expression) state)) state]
      [else (Mwhile expression (Mstate (then_s expression) state))]
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
      ((and (eq? var (caar state)) (null? (cdar state))) (error 'varnotassigned "using before assigning"))
      ((and (eq? var 'return) (eq? #t (car (cdar state)))) 'true)
      ((and (eq? var 'return) (eq? #f (car (cdar state)))) 'false)
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
      ((eq? (car expression) 'var) (Mdeclare expression state))
      ((eq? (car expression) 'if) (Mif (cdr expression) state))
      ((eq? (car expression) '=) (Massign (cdr expression) state))
      ((eq? (car expression) 'while) (Mwhile (cdr expression) state))
      ((eq? (car expression) 'return) (Mreturn (cadr expression) state)))))
      
      

; helper functions to abstract the operator and operands of binary expressions.
; here the assumption is the operator is "infix".  To change it so the operator is "prefix"
; or "postfix" only requires changing the helpers and not the main code above
(define operator
  (lambda (exp)
    (car exp)))
(define leftoperand cadr)
(define rightoperand caddr)

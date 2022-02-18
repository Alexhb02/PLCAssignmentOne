#lang racket
;Alex Bradley, Lucas Tilford, Andrew 

(require "simpleParser.rkt")

;returns the proper value by evaulating the syntax tree resulting from parsing the input file
(define interpreter
  (lambda (filename)
    (interpreter-help (parser filename) '((return)))
    ))

;helper function for the interpreter that evalutes each statement in the syntax tree while updating the state
(define interpreter-help
  (lambda (syntaxtree state)
    (cond
      [(null? syntaxtree) (get 'return state)]
      [else (interpreter-help (cdr syntaxtree) (Mstate (car syntaxtree) state))]
      )))

; evaluates the value of an expression
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
      (else (Mboolean expression state))))) ; check boolean value of expression
 
; evaluates the truth of a statement
(define Mboolean
  (lambda (expression state)
    (cond
      ((boolean? expression) expression)
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((exists? expression state) (get expression state))
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

; updates the state with the newly declared variable
(define Mdeclare
  (lambda (expression state)
    (cond
      ((not (null? (cdr (cdr expression)))) (cons (list (cadr expression) (Mvalue (caddr expression) state)) state))
      ((not (null? (cadr expression))) (cons (list (cadr expression)) state)))))

; updates the state variable with the newly assigned value
(define Massign
  (lambda (expression state)
    (cond
      ((exists? (car expression) state) (insert (car expression) (Mvalue (cadr expression) state) state))
      (else (error 'varnotdeclared "Var not declared")))))

; evauluates the value of an expression following a return keyword
(define Mreturn
  (lambda (expression state)
    (cond
      ((null? expression) (error `invalidreturn "Invalid return"))
      (else (insert 'return (Mvalue expression state) state)))))

; evaluates an if statement
(define Mif
  (lambda (expression state)
    (cond
      ((Mboolean (conditional expression) state) (Mstate (then_s expression) state))
      ((not (null? (optional_else expression))) (Mstate (else_s expression) state))
      (else state))))

;abstraction for if statement
(define optional_else cddr)

; evauluates a while statement
(define Mwhile
  (lambda (expression state)
    (cond
      [(not (Mboolean (conditional expression) state)) state]
      [else (Mwhile expression (Mstate (then_s expression) state))]
     )))

;abstractions for if and while statements 
(define then_s cadr)
(define conditional car)
(define else_s caddr)

; check whether a variable exists in the state (ie check if the variable has been declared)
(define exists?
  (lambda (var state)
    (cond
      ((pair? var) #f)
      ((null? state) #f)
      ((eq? var (caar state)) #t)
      (else (exists? var (cdr state))))))

; get the value of the variable stored in the state
(define get
  (lambda (var state)
    (cond
      ((null? state) (error 'varnotdeclared "The variable has not been declared"))
      ((and (eq? var (caar state)) (null? (cdar state))) (interpreter "11.txt")(error 'varnotassigned "using before assigning"))
      ((and (eq? var 'return) (eq? #t (car (cdar state)))) 'true)
      ((and (eq? var 'return) (eq? #f (car (cdar state)))) 'false)
      ((eq? var (caar state)) (car (cdar state)))
      (else (get var (cdr state))))))

; updates the value of the given variable in the state
(define insert
  (lambda (var value state)
    (cond
      ((null? state) (error 'varnotdeclared "The variable has not been declared"))
      ((eq? var (caar state)) (cons (cons (caar state) (list value))(cdr state)))
      (else (cons (car state) (insert var value (cdr state)))))))

; evaluates the given statement
(define Mstate
  (lambda (statement state)
    (cond
      ((eq? (car statement) 'var) (Mdeclare statement state))
      ((eq? (car statement) 'if) (Mif (cdr statement) state))
      ((eq? (car statement) '=) (Massign (cdr statement) state))
      ((eq? (car statement) 'while) (Mwhile (cdr statement) state))
      ((eq? (car statement) 'return) (Mreturn (cadr statement) state)))))
      
; helper functions to abstract the operator and operands of binary expressions.
(define operator
  (lambda (exp)
    (car exp)))
(define leftoperand cadr)
(define rightoperand caddr)


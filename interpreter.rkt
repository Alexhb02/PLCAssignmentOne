#lang racket
;Alex Bradley, Lucas Tilford, Andrew Gross

(require "simpleParser.rkt")

;returns the proper value by evaulating the syntax tree resulting from parsing the input file
(define interpreter
  (lambda (filename)
    (interpreter-help (parser filename) '(((return)))))
    )

;helper function for the interpreter that evalutes each statement in the syntax tree while updating the state (lambda(v) (interpreter-help (cdr v))
(define interpreter-help
  (lambda (syntaxtree state)
    (cond
      [(null? syntaxtree) (get 'return state)]
      [(eq? (caar syntaxtree) 'return) (get 'return (Mstate (car syntaxtree) state (lambda(v) (interpreter-help (cdr syntaxtree) v)) (lambda (v) v)))]
      [else (interpreter-help (cdr syntaxtree) (Mstate (car syntaxtree) state (lambda(v) (interpreter-help (cdr syntaxtree) v)) (lambda (v) v)))])))

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
      ((not (null? (cdr (cdr expression)))) (cons (cons (list (cadr expression) (Mvalue (caddr expression) state)) (car state)) (cdr state)))
      ((not (null? (cadr expression))) (cons (cons (list (cadr expression)) (car state)) (cdr state))))))


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
      ((null? (get `return state)) (insert 'return (Mvalue expression state) state))
      (else state))))

; evaluates an if statement
(define Mif
  (lambda (expression state next break)
    (cond
      ((Mboolean (conditional expression) state) (Mstate (then_s expression) state next break))
      ((not (null? (optional_else expression))) (Mstate (else_s expression) state next break))
      (else state))))

;abstraction for if statement
(define optional_else cddr)

; evauluates a while statement
(define Mwhile
  (lambda (expression state next oldbreak)
    (letrec ((break (lambda (v) 
                      (next v))))
    (loop expression state next break))))

(define loop
  (lambda (expression state next break)
    (letrec ((repeat (lambda (v)
                       (loop expression v next break))))
    (cond
      [(not (Mboolean (conditional expression) state)) (next state)]
      [else (Mstate (then_s expression) state repeat break)]))))

;abstractions for if and while statements 
(define then_s cadr)
(define conditional car)
(define else_s caddr)

(define exists?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((null? (cdr state)) (existsHelper var (car state)))
      ((eq? (existsHelper var (car state)) #t) #t)
      (else (exists? var (cdr state))))))

; check whether a variable exists in the state (ie check if the variable has been declared)
(define existsHelper
  (lambda (var state)
    (cond
      ((pair? var) #f)
      ((null? state) #f)
      ((eq? var (caar state)) #t)
      (else (existsHelper var (cdr state))))))

(define get
  (lambda (var state)
    (cond
      ((null? state) 'false)
      ((null? (cdr state)) (getHelper var (car state)))
      [(existsHelper var (car state)) (getHelper var (car state))]
      [else (get var (cdr state))])))
; get the value of the variable stored in the state
(define getHelper
  (lambda (var state)
    (cond
      ((null? state) (error 'varnotdeclared "The variable has not been declared"))
      ((and (and(eq? var (caar state)) (eq? var 'return)) (not (null? (cdar state))) (cadar state)))
      ((and (eq? var (caar state)) (eq? var 'return)) (cdar state))
      ((and (eq? var (caar state)) (null? (cdar state)))(error 'varnotassigned "using before assigning"))
      ((and (eq? var 'return) (eq? #t (car (cdar state)))) 'true)
      ((and (eq? var 'return) (eq? #f (car (cdar state)))) 'false)
      ((eq? var (caar state)) (car (cdar state)))
      (else (getHelper var (cdr state))))))

(define insert
  (lambda (var value state)
    (cond
      [(null? state) (error 'varnotdeclared "The variable has not been declared")]
      [(existsHelper var (car state)) (list (insertHelper var value (car state)))]
      [else (cons (car state) (insert var value (cdr state)))])))
; updates the value of the given variable in the state
(define insertHelper
  (lambda (var value state)
    (cond
      ((null? state) #f)
      ((eq? var (caar state)) (cons (cons (caar state) (list value))(cdr state)))
      (else (cons (car state) (insertHelper var value (cdr state)))))))

; evaluates the given statement

(define Mstate
  (lambda (statement state next break)
    (cond
      ((eq? (car statement) 'var) (Mdeclare statement state))
      ((eq? (car statement) 'if) (Mif (cdr statement) state next break))
      ((eq? (car statement) '=) (Massign (cdr statement) state))
      ((eq? (car statement) 'while) (Mwhile (cdr statement) state next break))
      ((eq? (car statement) 'begin) (Mblock (cdr statement) (cons '() state) next break))
      ((eq? (car statement) 'return) (Mreturn (cadr statement) state)))))

(define Mblock
  (lambda (actions layers next break)
    (cond
      [(null? actions) (cdr layers)]
      [else (Mblock (cdr actions) (Mstate (car actions) layers next break) next break)]
     )))

; helper functions to abstract the operator and operands of binary expressions.
(define operator
  (lambda (exp)
    (car exp)))
(define leftoperand cadr)
(define rightoperand caddr)
#lang racket

; insert places an input number in an ordered list in a manner that maintains said ascending order
(define insert
  (lambda (a list)
    (cond
      [(null? list) list]
      [(> (car list) a) (cons a list)]
      [else (cons (car list) (insert a (cdr list)))])))

; removedups removes all atoms in a list that precedes an atom that is the same.
(define removedups
  (lambda (mylist)
    (cond
      [(null? (cdr mylist)) (list (car mylist))]
      [(eq? (car mylist) (car (cdr mylist))) (removedups (cons (car mylist) (cdr (cdr mylist))))]
      [else (cons (car mylist) (removedups (cdr mylist)))])))


; nestlist takes an input list and recursively creates a list with the element after it so that the last element may be nested in multiple lists
(define nestlist
  (lambda (mylist)
  (cond
    [(null? mylist) mylist]
    [(null? (cdr mylist)) mylist]
    [else (list (car mylist) (nestlist (cdr mylist)))])))

; deepcons removes an input element from a list and any sublists within the list
(define deepcons
  (lambda (a mylist)
    (cond
      [(list? (car mylist)) (cons (deepcons a (car mylist)) (cdr mylist))]
      [else (cons a mylist)])))

; nestlistfront takes an input list and recursively creates a list with the element after it so that the first element may be nested in multiple lists
(define nestlistfront
  (lambda (mylist)
    (nestlistfronthelp (myreverse mylist))))

(define nestlistfronthelp
  (lambda (mylist)
    (cond
      [(null? (cdr mylist)) (list(car mylist))]
      [else (list (nestlistfronthelp (cdr mylist)) (car mylist))])))

; this function appends to list together
(define myappend
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (myappend (cdr l1) l2)))))

; this function reverses a list
(define myreverse
  (lambda (lis)
    (if (null? lis)
        '()
        (myappend (myreverse (cdr lis)) (cons (car lis) '())))))

; numparens* takes an input list and finds the number of pairs of parentheses in the entire list
(define numparens*
  (lambda (mylist)
    (cond
      [(null? mylist) 1]
      [(list? (car mylist)) (+ (numparens* (cdr mylist)) (numparens* (car mylist)))]
      [else (numparens* (cdr mylist))])))

;dup* duplicates all of the elements in the input list, both atoms and sublists alike
(define dup*
  (lambda (mylist)
    (cond
      [(null? mylist) mylist]
      [(list? (car mylist)) (cons (dup* (car mylist)) (dup*(cdr mylist)))]
      [else (cons (car mylist) (cons (car mylist) (dup* (cdr mylist))))])))


;removedups* removes all of the two consecutive atoms that equal each other, even within sublists of the input list.
(define removedups*
  (lambda (mylist)
    (cond
      [(null? mylist) mylist]
      [(list? (car mylist)) (cons (removedups* (car mylist)) (removedups* (cdr mylist)))]
      [(null? (cdr mylist)) (cons (car mylist) (cdr mylist))]
      [(eq? (car mylist) (car (cdr mylist))) (removedups*(cons (car mylist) (cdr(cdr mylist))))]
      [else (cons (car mylist) (removedups*(cdr mylist)))]
      )))

; removedups** removes all of the duplicate that removes repeated element in any list or sublist and then removes any repeated sublists
(define removedups**
  (lambda (mylist)
    (if (equal? (removedups* mylist) (removedupshelp** (removedups* mylist)))
        (removedups* mylist)
        (removedups** (removedupshelp** (removedups* mylist))))))

(define removedupshelp**
  (lambda (mylist)
    (cond
      [(null? mylist) mylist]
      [(not (list? mylist)) mylist]
      [(null? (cdr mylist)) (list (removedupshelp** (car mylist)))]
      [else (cons (car mylist) (removedupshelp** (cdr mylist)))])))
  
;transpose transposes a list of lists in a similar fashion to a matrix.
(define transpose
  (lambda (mylist)
    (if (null? (car mylist))
      '()
      (cons (mymap car mylist)(transpose (mymap cdr mylist))))))


; mymap: takes a function and a list and applies the function to each element in the list
; (mymap (lambda (x) (* x x)) '(1 3 5 9))  => (1 9 25 81)
(define mymap
  (lambda (f lis)
    (if (null? lis)
        '()
        (cons (f (car lis)) (mymap f (cdr lis))))))
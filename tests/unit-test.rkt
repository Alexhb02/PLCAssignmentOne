#lang racket/base

; ; CSDS:345 Mark Wang
; ; README
; ; This unit test file assumes 4 things
; ; 1. That the module is inside the same directory that contains the test folder
; ; 2. That there is a line in the code that resembles the following:
; ;    (provide interp)
; ; 3. The interp function will take in a syntax tree, and an empty list indicating the state and return a value
; ; 4. A value constitutes either a number, 'true, 'false

; ; The module will print nothing if all assertions are correct, but will print a file error otherwise.

; ; Feel free to change the filename inside the double quotes to the name of your module file.
(require rackunit 
  "../interpreter.rkt"
  "simpleParser.rkt")

; ; Number Tests
(check-equal? (interp (parser "1.txt")) 150)
(check-equal? (interp (parser "2.txt")) -4)
(check-equal? (interp (parser "3.txt")) 10)
(check-equal? (interp (parser "4.txt")) 16)
(check-equal? (interp (parser "5.txt")) 220)
(check-equal? (interp (parser "6.txt")) 5)
(check-equal? (interp (parser "7.txt")) 6)
(check-equal? (interp (parser "8.txt")) 10)
(check-equal? (interp (parser "9.txt")) 5)
(check-equal? (interp (parser "10.txt")) -39)

; ; Error Tests
; ; NOTE THAT THIS DOES NOT TEST THAT YOU THROW THE CORRECT ERRORS
(check-exn exn:fail? (lambda () (interp (parser "11.txt"))))
(check-exn exn:fail? (lambda () (interp (parser "12.txt"))))
(check-exn exn:fail? (lambda () (interp (parser "13.txt"))))
(check-exn exn:fail? (lambda () (interp (parser "14.txt"))))

; ; Boolean, If, While Tests
(check-equal? (interp (parser "15.txt")) 'true)
(check-equal? (interp (parser "16.txt")) 100)
(check-equal? (interp (parser "17.txt")) 'false)
(check-equal? (interp (parser "18.txt")) 'true)
(check-equal? (interp (parser "19.txt")) 128)
(check-equal? (interp (parser "20.txt")) 12)

; ; Extra Challenge
; (check-equal? (interp (parser "21.txt")) 30)
; (check-equal? (interp (parser "22.txt")) 11)
; (check-equal? (interp (parser "23.txt")) 1106)
; (check-equal? (interp (parser "24.txt")) 12)
; (check-equal? (interp (parser "25.txt")) 16)
; (check-equal? (interp (parser "26.txt")) 72)
; (check-equal? (interp (parser "27.txt")) 21)
; (check-equal? (interp (parser "28.txt")) 164)

; ; Project 2 Tests

; ; Block, Return, State Tests
(check-equal? (interp (parser "29.txt")) 20)
(check-equal? (interp (parser "30.txt")) 164)
(check-equal? (interp (parser "31.txt")) 32)
(check-equal? (interp (parser "32.txt")) 2)
(check-equal? (interp (parser "34.txt")) 25)
(check-equal? (interp (parser "35.txt")) 21)

; ; Continue, Break Tests
(check-equal? (interp (parser "36.txt")) 6)
(check-equal? (interp (parser "37.txt")) -1)
(check-equal? (interp (parser "38.txt")) 789)
(check-equal? (interp (parser "42.txt")) 12)

; ; Error Tests
(check-exn exn:fail? (lambda () (interp (parser "33.txt"))))
(check-exn exn:fail? (lambda () (interp (parser "39.txt"))))
(check-exn exn:fail? (lambda () (interp (parser "40.txt"))))
(check-exn exn:fail? (lambda () (interp (parser "41.txt"))))
(check-exn exn:fail? (lambda () (interp (parser "47.txt"))))

; ; Try/Catch/Finally Tests
(check-equal? (interp (parser "43.txt")) 125)
(check-equal? (interp (parser "44.txt")) 110)
(check-equal? (interp (parser "45.txt")) 2000400)
(check-equal? (interp (parser "46.txt")) 101)

; ; Extra Challenge
; (check-equal? (interp (parser "48.txt")) 21)

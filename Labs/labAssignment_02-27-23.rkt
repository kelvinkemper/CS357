#lang racket

;; Kelvin Kemper 
;; Recitation Assignment 02-27-23

(define fst (lambda (x y) x))
(define const (curry fst))
;; (((curry fst) 1) 2) ==> ((const 1) 2) == 1
(define swapc (lambda (f) (lambda (y) (lambda (x) ((f x) y)))))
(define compc (curry compose))

;; 1. 4th-of-7 - curried function that takes 7 arguments and returns the 4th one
;;    Use only const, swapc
 ;(define 2nd-of-3 (const const))
 ;(((2nd-of-3 'a) 'b) 'c)

 ;(define 1st-of-3 (swapc (const const)))
 ;(((1st-of-3 'a) 'b) 'c)

(define 4th-of-7 (const (const (const (swapc (const (swapc (const const))))))))
;(((((((4th-of-7 1) 2) 3) 4) 5) 6) 7)

; (((((((4th-of-7 'a) 'b) 'c) 'd) 1) 2) 3)
;; 'd

;; 2. comp3 - curried function that takes 3 functions as (curried) arguements and composes them
;;    Use only compc
;; (define compc (curry compse))

(define comp1 (compc))
;((comp2 add1) 5) ==> 6

(define comp3 (compc))

;(((((comp3 add1) add1 ) add1 ) add1) 5)
;; 8
;; ((((comp3 square) sub1) square) 2)
;; 9

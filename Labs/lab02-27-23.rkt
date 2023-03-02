#lang racket

(define curry 
 (lambda (f)
    (lambda (x)
     (lambda (y)
        (f x y)))))

(define uncurry
    (lambda (f)
        (lambda (x y)
            ((f x) y))))

(define +c (curry +))
(define my-+ (uncurry +c))

(define f1 (compose (+c 3) (+c  5)))
;; same as basically (+c 8)

(define fst (lambda (x y) x))
(define snd (lambda (x y) y))

(define const (curry fst))
;;((const 3) 2)
;; takes in some value as an argument and returns the constant value of that argument
(define f3 (const 50))

;; ((const 'a) 100)
;; (fst 'a 100)



(define f4 (curry snd))

(define f5 (const identity))
;; always returns the idenitiy function

(define 2nd-of-3 (const const))
;; (f6 x) ==> const
;; ((f6 x) y) ==> (const y)
;; (((f6 x) y) z) ==> y
;; ((const y) z)

(define 3rd-of-4 (const (const const)))
; (g w) ===> (const const) = 2nd-of-3
; ((((g w) x) y) z) ==> y

(define swapc (lambda (f) (lambda (y) (lambda (x) ((f x) y)))))
;; takes in 3 arguments, evaluates f x, and returns y

(define 1st-of-3 (swapc 2nd-of-3))

(define compc (curry compose))
(((compc add1) add1) 3)

(define comp-add1 (compc add1))
;; ((comp-add1 identity) 3) ==>

;; (expt a b) ==> a^b
(define expc (curry expt))

;; powers of 2 function
(define power2 (expc 2))

;; squares any number
(define square ((swapc expc) 2))
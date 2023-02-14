#lang racket

;; variadric functions

;;max of 2 arguments
(define max2
  (lambda (x y)
    (if (> x y) x y)))

;; find the max of elemtns in a list
(define max-list
  (lambda (ls)
    (if (null? (cdr ls))
        (car ls)
      (max2 (car ls)
            (max-list (cdr ls))))))

(define max
  (lambda args
    (letrec
      ((singleton?
        (lambda (ls) (null? (cdr ls))))
      (max2
        (lambda (x y)
            (if (> x y) x y)))
      (max-list
        (lambda (ls)
          (if (null? (cdr ls))
              (car ls)
              (max2 (car ls)
                 (max-list (cdr ls)))))))
    (max-list args))))

;; (apply let's you apply functions that are trapped in a list)


;; map function - 
;; can apply a function to a list of numbers

(define squares
  (lambda (ls)
    (if (null? ls)
        '()
      (cons (* (car ls) (car ls))
            (squares (cdr ls))))))

;; or we can use map
;;(map (lambda (x) (* x x)) '(1 2 3 4))

;; how can we add 7 to everything in a list
;; (map (lambda (x) (+ x 7)) '(1 2 3 4))

(define map
  (lambda (proc ls)
    (if (null? ls)
        '()
      (cons (proc (car ls))
            (map proc (cdr ls))))))


;; (map + '(1 2 3) '(4 5 6) '(7 8 9))
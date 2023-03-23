#lang racket

(define length
  (lambda (ls)
    (if (null? ls)
        0
        (+ 1 (length (cdr ls))))))

;; acc = accumulator, becomes the answer to the problem
;; it = iterator or iteration

(define length-it
  (lambda (ls acc)
    (if (null? ls)
        acc
        (length-it (cdr ls) (+ acc 1)))))

(define length2
  (lambda (ls)
    (length-it ls 0)))


;; append is linear O to the first list
(define reverse
  (lambda (ls)
    (if (null? ls)
        '()
        (append (reverse (cdr ls))
                (list (car ls))))))

;; (reverse-it '(1 2 3) '())
;; (reverse-it '(2 3 ) '(1))
;; (reverse-it '(3) '(2 1))
;; (reverse-it '() '(3 2 1))
(define reverse-it
  (lambda (ls acc)
    (if (null? ls)
        acc
        (reverse-it (cdr ls) (cons (car ls) acc)))))

(define append
  (lambda (ls0 ls1)
    (if (null? ls0)
        ls1
        (cons (car ls0)
              (append (cdr ls0) ls1)))))

(define append-recur
  (lambda (ls0 ls1
    (letrec
      ((loop
          (lambda (ls acc)
            (if (null? ls)
                acc
                (loop (cdr ls) (cons (car ls) acc)))))
      (loop (loop ls0 '()) ls1))))))
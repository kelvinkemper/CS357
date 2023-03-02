#lang racket

;; write a function that determines if a list is a set
(define lat '(apple peach pear peach plum apple lemon))

(define member?
  (lambda (x ls)
    (cond
      ((null? ls) #f)
      ((equal? x (car ls)) #t)
      (else
        (member? x (cdr ls)))))) 

(define set?
    (lambda (ls)
      (cond
        ((null? ls) #t)
        ((member? (car ls) (cdr ls)) #f)
        (else
            (set? (cdr ls))))))     


 ;; makes a nonset into a set       
(define makeset
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((member? (car ls) (cdr ls))
          (makeset (cdr ls)))
      (else (cons (car ls) (makeset (cdr ls)))))))

;; (makeset '(1 2 3 3 4))
;; (member? 1 '(2 3 3 4)) ==> false ==> cons 1 ==> '(1) '(2 3 3 4)
;; (member? 2 '(2 3 4)) ==> false ==> cons '(1) 2 ==> '(3 3 4)
;; (member? 3 '(3 4)) ==> true ==> recurse
;; (member? 3 '(4) ==> false ==> cons '(1 2 3) & '(4) 

(define subset?
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) #t)
      ((member? (car ls1) ls2) 
            (subset? (cdr ls1) ls2))
      (else
        #f))))

(define subset?-and
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) #t)
      (else
        (and (member? (car ls1) ls2)
            (subset? (cdr ls1) ls2))))))

;; brute force way :)
(define eqset?
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) #t)
      ((equal? (car ls1) (car ls2))
           (eqset? (cdr ls1) (cdr ls2)))
      (else
        #f))))

;; short and brilliant way
(define eqset
  (lambda (ls1 ls2)
    (cond
      ((subset? ls1 ls2)
        (subset? ls2 ls1))
      (else
        #f))))

;;;;;;;;;;;-----------------;;;;;;;;;;;;
;;currying

(define eq?-c
  (lambda (x)
    (lambda (a)
      (eq? a x))))

(define k 'salad)
(define eq?-salad (eq?-c k))

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? (car l) a) (cdr l))
      (else (cons (car l) (rember-f test? a (cdr l)))))))


;;;;;
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond 
        ((null? l) '())
        ((test? (car l) old)
         (cons new (cons old (cdr l))))
        (else (cons (car l)
                    ((insertL-f test?) new old (cdr l))))))))
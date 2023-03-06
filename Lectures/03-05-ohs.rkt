#lang racket

(define fold
      (lambda (seed proc)
        (letrec
          ((pattern
            (lambda (ls)
              (if (null? ls)
                  seed
                  (proc (car ls)
                        (pattern (cdr ls)))))))
          pattern)))

;; (define my-length (lambda (ls) _____))


(define my-length (fold 0 (lambda (x prev-len) (+ 1 prev-len))))

(define member?
  (lambda (item ls)
      (if (null? ls) #f (if (eq? item (car ls)) #t (member? item (cdr ls))))
    )
)

(define delete-helper
  (lambda (item ls0)
    (begin
      (display "delete-helper has been called with arguments: ")
      (display item)
      (display " ")
      (display ls0)
      (display "\n")
    (if (member? item ls0)
        ls0
        (cons item ls0))
    )
    )
  )

(define delete-duplicates (fold '() delete-helper))

(define tail-recur
  (lambda (bproc xproc aproc acc0)
    (letrec
        ((loop
          (lambda (x acc)
            (if (bproc x)
                acc
                (loop (xproc x) (aproc x acc))))))
      (lambda (x) (loop x acc0)))))

;; (define fact (tail-recur zero? sub1 * 1))

;; (assoc 'b '((a 1) (b 2) (c 3) (b 4))) ===> '(b 2)
;; (assoc 'b '((a b) (c d) (e f))) ===> #f
;; (assoc-b '((a b) (c d) (e f))) ===> #f
;; ...

;; (assoc 'b ls)
;;    the first element is '(a 500)
;;    (assoc 'b (cdr ls)) is '(b abcde)
;;    ====> '(b abcde)

;; (assoc 'b ls)
;;    the first element is '(a 3)
;;    (assoc 'b (cdr ls)) is '(b 100)
;;    ===> '(b 100)

;; (assoc 'b ls)
;;    the first element is _____
;;    (assoc 'b (cdr ls)) is ____
;;    ====> _____

;; what do we need to check?
;; - we can check if the car of the first element equals 'b ==> return the first element (pair)
;; - otherwise, ...

(define assoc-b-helper
  (lambda (fst assoc-rest-of-list)
    (if (eq? (car fst) 'b) fst assoc-rest-of-list)
  )
)

(define assoc-b (fold #f assoc-b-helper))

;;(define assoc
;;  (lambda (x ls)
;;    (let ((assoc-helper (lambda (fst assoc-rest-of-list) _________))
;;          (assoc-final (fold #f assoc-helper)))
;;      (assoc-final ls))
;;  )
;;)

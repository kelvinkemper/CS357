#lang racket

;; referential transparency
;; first class fucntions
;; program data equivalance

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(atom? 'turkey)
(atom? 'atom?)





#lang racket

(let ((x 3))
    (+ x (* 10 x)))
;; 33

(define x 5)
(+ x
   (let ((x 3))
     (+ x (* 10 x))))

;; 38


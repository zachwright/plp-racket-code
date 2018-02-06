#lang racket


;------- Session 09 -------;

;------- opening exercise -------;
(define any-bigger-than?
  (lambda (n lon)
    (> (apply max '(26 37 41 25 12)) 40)
    ))

;------- recursive function for power -------;
(define power
  (lambda (x n)
    (if (zero? n)
        1
        (* x (power x (sub1 n))))))

;------- recursive function for list-length -------;
(define list-length
  (lambda (lon)
    (if (null? lon)
        0
        (+ 1 (list-length (rest lon))) )))


;------- recursive function for any-bigger-than? -------;
(define rec-any-bigger-than?
  (lambda (n lon)
    (if (null? lon)
        #f
        (or (> (first lon) n)
        (rec-any-bigger-than? n (rest lon))) ) ))

;------- recursive function for remove-first -------;
(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eq? s (first los))
            (rest los)
            (cons (first los)
                  (remove-first s (rest los)))))))
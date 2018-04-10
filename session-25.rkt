#lang racket

;; russian peasant multiplication -v1
(define multiply
  (lambda (m n)
    (display-line m n)
    (if (= m 1)
        n
        (multiply (half m) (double n)))))



(define half (lambda (n) (/ n 2)))
(define double (lambda (n) (* n 2)))



(define display-line
  (lambda (m n)
    (display m)
    (display " ")
    (display n)
    (newline)))

(multiply 16 43)

;; like map
(define for-each
  (lambda (proc lst)
    (if (null? lst)
        (void)
        (begin
          (proc (first lst))
          (for-each proc (rest lst))))))

(define displayln
  (lambda lst
    (for-each display lst)
    (newline)))

(define hypotenuse
  (lambda (m n)
    (sqrt (+ (* m m) (* n n)))))

(define right-triangle
  (lambda ()
    (let ((x (read-num "The first side: "))
          (y (read-num "The second side: ")))
      (displayln "The hypotenuse is:"
    (hypotenuse x y)))))

(define read-num
  (lambda (prompt)
    (display prompt)
    (display " ")
    (let ((answer (read)))
      (if (number? answer)
          answer
          (begin (displayln "Illegal value")
                 (read-num prompt)))))) ;; Tail recursive

;; Not sure this one is working right
(define read-type
  (lambda (type?)
    (letrec ((reader (lambda (prompt)
                       (display prompt)
                       (display " ")
                       (let ((answer (read)))
                         (if (type? answer)
                             answer
                             (begin (displayln "Illegal value")
                                    (reader prompt)))))))
      reader)))

(define balance 100)
(define withdraw
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        (error "Insufficient funds" balance))))
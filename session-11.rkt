#lang racket
; ---------------------------
; mutually recursive functions

(define map-nlist
  (lambda (f nlst)
    (if (null? nlst)
        '()
        (cons (map-numexpr f (first nlst))
              (map-nlist f (rest nlst)))
        )
    ))


(define map-numexpr
  (lambda (f numexpr)
    (if (number? numexpr)
        (f numexpr)
        (map-nlist f numexpr)
        )
    ))

; (map-nlist add1 '(1 (4 (9 (16 25)) 36 49) 64))
; '(2 (5 (10 (17 26)) 37 50) 65)


; program derivation example
; (after the merging of the two functions)
; in the plus operator is where the other function call was

(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (if (symbol? (first slist))
               (if (eq? s (first slist)) 1 0)
               (count-occurrences s (first slist)))
           (count-occurrences s (rest slist))))))




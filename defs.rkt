#lang racket

(define (butlast str)
  (substring str 0 (- (string-length str) 1)))
(define (bl str)
  (substring str 0 (- (string-length str) 1)))

(define (butfirst seq)
  (sequence-tail seq 1))
(define (bf seq)
  (sequence-tail seq 1))

;; define word
;; define se

(define (member? item seq)
  (sequence-ormap (lambda (x)
                    (equal? item x))
                  seq))

;; 1.31
(define (identity x) (+ x 0))
(define (next x) (+ x 1))

(define (product f g a b)
    (if (> a b)
        1
        (* (f a) (product f g (g a) b))))
 (define (product-iter f g a b) 
    (define (iter a result)
      (if (> a b)
          result
          (iter (g a) (* result (f a)))))
    (iter a 1))
 
 ;; 1.32
 (define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b))))
 (define (accumulate-iter combiner null-value term a next b)
    (define (iter a result)
      (if (> a b)
          result
          (iter (next a) (combiner result (term a)))))
      (iter a null-value))
 
 


#lang racket

(define (butlast str)
  (substring str 0 (- (string-length str) 1)))
(define (bl str)
  (substring str 0 (- (string-length str) 1)))

(define (butfirst str)
  (substring str 1 (string-length str)))
(define (bf str)
  (substring str 1 (string-length str)))

(define (member? u lst)
    (not (equal? (member u) #f)))



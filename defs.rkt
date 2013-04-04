#lang racket

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

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
 
 (define (average x y)
   (/ (+ x y) 2))
 
 (define (make-rat n d)
   (let ((g (gcd n d)))
     (cons (/ n g) (/ d g))))
 (define (numer x)
   (car x))
 (define (denom x)
   (cdr x))
 (define (add-rat a b)
   (make-rat (+ (* (numer a) (denom b)) (* (numer b) (denom a)))
             (* (denom a) (denom b)))) 
 (define (sub-rat a b)
   (make-rat (- (* (numer a) (denom b)) (* (numer b) (denom a)))
             (* (denom a) (denom b))))
 (define (mul-rat a b)
   (make-rat (* (numer a) (numer b))
             (* (denom a) (denom b))))
 (define (div-rat a b)
   (make-rat (* (numer a) (denom b))
             (* (denom a) (numer b))))
 (define (equal-rat? a b)
   (= (* (numer a) (denom b))
      (* (numer b) (denom a))))
 (define (print-rat x)
   (display (numer x))
   (display "/")
   (display (denom x)))
 
 (define (reverse items)
   (define (iter items result)
     (if (null? items)
         result
         (iter (cdr items) (cons (car items) result))))
   (iter items null)) 
 (define (deep-reverse items)
   (if (pair? items)
       (reverse (map deep-reverse items))
       items))
 
(define (tree-map f tree)
    (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (tree-map f sub-tree)
               (f sub-tree)))
         tree))

;; Polynomial Multiplier
;; Copyright (C) 2026  Tanmay Rai

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;; Procedure for adding two polynomials

(define (polynomial-add p q)
  (cond ((null? p) q)
	((null? q) p)
	(else (cons (+ (car p) (car q))
		    (polynomial-add (cdr p) (cdr q))))))

;; Procedure for scaling a polynomial
(define (const-mul p k) (map (lambda (x) (* x k)) p)) 

;; Procedure for multiplying two polynomials
(define (polynomial-mul p q)
  (define (slide p n)
    (define (iter counter n result)
      (if (= counter n) (append result p)
	  (iter (+ counter 1) n (cons 0 result))))
    (iter 0 n '()))
  (define (polynomial-iter q i result)
    (if (null? q) result
	(polynomial-iter (cdr q) (+ i 1) (polynomial-add result (slide (const-mul p (car q)) i)))))
  (polynomial-iter q 0 '()))


;; Examples -

;; Suppose p(x) = 1 + x + x^2 and q(x) = -2 + x^3 then p = (1 1 1) and q = (-2 0 0 1)

(define p (list 1 1 1))
(define q (list -2 0 0 1))

;; Adding those polynomials
(polynomial-add p q)

;; Multiplying p by 3
(const-mul p 3)

;; Multiplying p and q
(polynomial-mul p q)




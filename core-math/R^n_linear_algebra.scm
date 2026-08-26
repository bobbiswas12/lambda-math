;; Linear Algebra Implementation: Generalization to Rⁿ
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

;; Vectors

(define (make-vector . xs) xs)

(define vector-1 (make-vector 1 2 3 4))

(define (dimension vector) (length vector))
(dimension vector-1)

(define (component i vector)
  (cond ((> i (dimension vector))
	 (display "The i-th component doesn't exist"))
	(else
	 (define (iter value vector)
	   (if (= value i)
	       (car vector)
	       (iter (+ 1 value) (cdr vector))))
	 (iter 1 vector))))

(define (equi-dim v₁ v₂)
  (= (dimension v₁) (dimension v₂)))

(define (vector-add v₁ v₂)
  (cond ((equi-dim v₁ v₂)
	 (if (null? v₁) '()
	     (map + v₁ v₂)))
	(else (display "Unequal Dimensional Vector Addition Error"))))

(define (scale-vector vector scaling-factor)
  (map (lambda (x) (* x scaling-factor)) vector))

(define (vector-sub v₁ v₂)
  (vector-add v₁ (scale-vector v₂ -1)))

(define (dot-product v₁ v₂)
  (cond ((equi-dim v₁ v₂)
	 (if (null? v₁) 0
	     (+ (* (car v₁) (car v₂)) (dot-product (cdr v₁) (cdr v₂)))))))

(define (norm∞ vector)
  (if (= (dimension vector) 0) 0
      (let ((vector-new (map abs vector)))
	(define (iter current  vector-1)
	  (if (null? vector-1) current
	      (if (>= current (car vector-1)) (iter current (cdr vector-1))
		  (iter (car vector-1) (cdr vector-1)))))
	(iter (car vector-new) (cdr vector-new)))))

(define (norm₂ vector)
  (sqrt (dot-product vector vector)))

(define (norm₁ vector)
  (if (= (dimension vector) 0) 0
      (if (null? vector) 0
	  (+ (abs (car vector)) (norm₁ (cdr vector))))))

(define (distance norm v₁ v₂)
  (norm (vector-sub v₁ v₂)))

(define (projection vector-1 vector-2)
  (let ((denominator (norm₂ vector-2)))
    (if (= 0 denominator)
	(display "Projection not defined")
	(let ((scaling-factor₁ (dot-product vector-1 (scale-vector vector-2 (/ 1 denominator)))))
	  (scale-vector (scale-vector vector-2 (/ 1 denominator)) scaling-factor₁)))))

(define (print-column-vector vector)
  (define (iter vector)
    (if (null? vector) '()
	(begin
	  (display (car vector))
	  (newline)
	  (iter (cdr vector)))))
  (iter vector))

(define (print-row-vector vector)
  (define (iter vector)
    (if (null? vector) (begin
			 (newline)
			 '())
	(begin
	  (display (car vector))
	  (display " ")
	  (iter (cdr vector)))))
  (iter vector))

(print-column-vector vector-1)
(print-row-vector vector-1)

;; Matrices ∈ Mᵐˣⁿ(R)

(define (Matrix . xs) xs)

(define (mutable? M₁ M₂)
  (and (equi-dim M₁ M₂) (equi-dim (car M₁) (car M₂))))

(define (Matrix-scale M₁ k)
  (map (lambda (x) (scale-vector x k)) M₁))

(define (Matrix-add M₁ M₂)
  (cond ((mutable? M₁ M₂)
	 (map (lambda (x y) (vector-add x y)) M₁ M₂))
	(else
	 (display "Addition Not defined!"))))

(define (Matrix-sub M₁ M₂)
  (Matrix-add M₁ (Matrix-scale M₂ -1)))

(define (Matrix-transpose M)
  (if (null? (car M)) '()
      (cons (map car M) (Matrix-transpose (map cdr M)))))

(define (Matrix-multiplicable? M₁ M₂)
  (= (dimension M₁) (dimension (car M₂))))

(define (Matrix-product M₁ M₂)
  (if (Matrix-multiplicable? M₁ M₂)
      (let ((M₁ᵀ (Matrix-transpose M₁)))	     
	(Matrix-transpose (map (lambda (vᵢ)
	       (map (lambda (vⱼ)
		      (dot-product vᵢ vⱼ))
		    M₂))
	     M₁ᵀ)))
      (display "Not Defined!")))

(define I₃ₓ₃ (Matrix (make-vector 1 0 0) (make-vector 0 1 0) (make-vector 0 0 1)))

(define (print-matrix M₁)
  (if (= 1 (dimension M₁)) (print-column-vector (car M₁))
      (let ((M₁ᵀ (Matrix-transpose M₁)))
	(map (lambda (x) (print-row-vector x)) M₁ᵀ))))
  

  

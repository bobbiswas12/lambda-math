;; Linear Algebra Implementation
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


;; Vectors in R²
(define (make-vector x y) (cons x y))

(define (x-component-vector vector) (car vector))
(define (y-component-vector vector) (cdr vector))

(define (vector-add vector-1 vector-2)
  (make-vector (+ (x-component-vector vector-1) (x-component-vector vector-2))
	       (+ (y-component-vector vector-1) (y-component-vector vector-2))))

(define (scale-vector vector scaling-factor)
  (let ((x (x-component-vector vector))
	(y (y-component-vector vector)))
    (make-vector (* scaling-factor x) (* scaling-factor y))))

(define vector-1 (make-vector 1 2))

(define (vector-sub vector-1 vector-2) (vector-add vector-1 (scale-vector vector-2 -1)))

(define (dot-product vector-1 vector-2)
  (let ((x₁ (x-component-vector vector-1))
	(y₁ (y-component-vector vector-1))
	(x₂ (x-component-vector vector-2))
	(y₂ (y-component-vector vector-2)))
    (+ (* x₁ x₂) (* y₁ y₂))))

(define (I-P₁ vector-1 vector-2) (dot-product vector-1 vector-2))

(define (norm I-P vector) (sqrt (I-P vector vector)))

(define (distance vector-1 vector-2)
  (norm I-P₁ (vector-sub vector-1 vector-2)))

(define (projection vector-1 vector-2)
  (let ((denominator (norm I-P₁ vector-2)))
    (if (= 0 denominator)
	(display "Projection not defined")
	(let ((scaling-factor₁ (dot-product vector-1 (scale-vector vector-2 (/ 1 denominator)))))
	  (scale-vector (scale-vector vector-2 (/ 1 denominator)) scaling-factor₁)))))

(projection (cons 3 4) (cons 10 0))

;; Matrices ∈ M²ˣ²(R)

(define (Matrix vector-1 vector-2) (list vector-1 vector-2))

(define (Matrix-add matrix-1 matrix-2)
  (let ((v₁ (car matrix-1))
	(v₂ (cadr matrix-1))
	(v₃ (car matrix-2))
	(v₄ (cadr matrix-2)))
    (list (vector-add v₁ v₃) (vector-add v₂ v₄))))

(define matrix-1 (Matrix vector-1 (cons 1 1)))
(define Identity (Matrix (cons 1 0) (cons 0 1)))
(Matrix-add matrix-1 matrix-2)

(define (scale-Matrix matrix-1 scaling-factor)
  (map (lambda (x) (scale-vector x scaling-factor)) matrix-1))

(define (transpose-matrix matrix-1)
  (let ((v₁₁ (x-component-vector (car matrix-1)))
	(v₂₁ (y-component-vector (car matrix-1)))
	(v₁₂ (x-component-vector (cadr matrix-1)))
	(v₂₂ (y-component-vector (cadr matrix-1))))
    (Matrix (make-vector v₁₁ v₁₂) (make-vector v₂₁ v₂₂))))

(transpose-matrix matrix-1)

(define (Matrix-product matrix-1 matrix-2)
  (let ((t-matrix-1 (transpose-matrix matrix-1)))
    (let ((v₁₁ (dot-product (car t-matrix-1) (car matrix-2)))
	  (v₁₂ (dot-product (car t-matrix-1) (cadr matrix-2)))
	  (v₂₁ (dot-product (cadr t-matrix-1) (car matrix-2)))
	  (v₂₂ (dot-product (cadr t-matrix-1) (cadr matrix-2))))
      (let ((V₁ (make-vector v₁₁ v₂₁))
	    (V₂ (make-vector v₁₂ v₂₂)))
	(Matrix V₁ V₂)))))

(define (print-matrix matrix)
  (let ((v₁₁ (x-component-vector (car matrix)))
	(v₂₁ (y-component-vector (car matrix)))
	(v₁₂ (x-component-vector (cadr matrix)))
	(v₂₂ (y-component-vector (cadr matrix))))
    (display v₁₁)
    (display " ")
    (display v₁₂)
    (newline)
    (display v₂₁)
    (display " ")
    (display v₂₂)))

(print-matrix (Matrix-product Identity matrix-1))

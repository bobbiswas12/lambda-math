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

(define (vector-add vector-1 vector-2) (make-vector (+ (x-component-vector vector-1) (x-component-vector vector-2)) (+ (y-component-vector vector-1) (y-component-vector vector-2))))

(define (scale-vector vector scaling-factor)
  (let ((x (x-component-vector vector))
	(y (y-component-vector vector)))
    (make-vector (* scaling-factor x) (* scaling-factor y))))

(define vector-1 (make-vector 1 2))

(define (vector-sub vector-1 vector-2) (vector-add vector-1 (scale-vector vector-2 -1)))

(define (|.| vector-1 vector-2)
  (let ((x₁ (x-component-vector vector-1))
	(y₁ (y-component-vector vector-1))
	(x₂ (x-component-vector vector-2))
	(y₂ (y-component-vector vector-2)))
    (make-vector (* x₁ x₂) (* y₁ y₂))))

(define (matrix-R² vector-1 vector-2) (list vector-1 vector-2))

(define (I-P vector-1 vector-2) 

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

(define (vector-add v₁ v₂)
  (if (null? v₁) '()
      (make-vector (+ (car v₁) (car v₂)) (vector-add (cdr v₁) (cdr v₂)))))
(vector-add vector-1 vector-1)

(define (scale-vector vector scaling-factor)
  (map (lambda (x) (* x scaling-factor)) vector))

(scale-vector vector-1 4)

(define (vector-sub v₁ v₂)
  (vector-add v₁ (scale-vector v₂ -1)))

(vector-sub vector-1 vector-1)

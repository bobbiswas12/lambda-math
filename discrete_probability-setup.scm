
;; Discrete Probability Setup
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


(define (make-probability-space sample-space probability-function) (cons sample-space probability-function))

(define (sample-space probability-space) (car probability-space))

(define (probability-function probability-space) (cdr probability-space))

(define (make-random-var probability-space random-var)
  (map (lambda (x) (random-var x)) (sample-space probability-space)))

(define (expectation-value probability-space random-var)
  (let ((new-probability-space (make-probability-space (make-random-var probability-space random-var) (probability-function probability-space))))
    (define (expectation-value-iter new-probability-space random-var result)
      (if (null? (sample-space new-probability-space)) result
	  (let ((rest-sample-space (cdr (sample-space new-probability-space)))
		(rest-probability-function (cdr (probability-function new-probability-space)))
		(sample-value (car (sample-space new-probability-space)))
		(prob-value (car (probability-function new-probability-space))))
	    (expectation-value-iter (make-probability-space rest-sample-space rest-probability-function) random-var (+ result (* sample-value prob-value))))))
    (expectation-value-iter new-probability-space random-var 0)))


;; Examples -

;; Defining a Bernoulli distribution
(define (bernoulli p)
  (let ((q (- 1 p)))
    (make-probability-space (list 0 1) (list q p))))

;; Finding out the expectation of the Bernoulli distribution
(expectation-value (bern 0.2) (lambda (x) x))

;; Implementing the famous prisonser's dilemmma problem
(define prisoners-dilemma (make-probability-space (list "CC" "CD" "DC" "DD") (list 0.25 0.25 0.25 0.25)))

(define random-var-pd (lambda (x) (cond ((string=? x "CC") 3)
					((string=? x "CD") 0)
					((string=? x "DC") 5)
					((string=? x "DD") 1))))

(define random-var-defect (lambda (x) (cond ((string=? x "CC") 0)
					    ((string=? x "CD") 0)
					    ((string=? x "DC") 5)
					    ((string=? x "DD") 1))))
(define random-var-cooperate (lambda (x) (cond ((string=? x "CC") 3)
					       ((string=? x "CD") 0)
					       ((string=? x "DC") 0)
					       ((string=? x "DD") 0))))

;; Finding out the expected number of coins upon ever defection
(expectation-value prisoners-dilemma random-var-defect)

;; Finding out the expected number of coins upon ever co-operation
(expectation-value prisoners-dilemma random-var-cooperate)


				      

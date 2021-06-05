;;   =======================
;;  == C H A P T E R   1 ==
;; =======================


;; Square root via Newton's method
;;
;; Starts with an initial guess of 1.0, then:
;;  - Squares the guess
;;  - Checks whether it's within 0.001 of the input value
;;  - If not, generates a new guess as the average of the bad guess and (input / bad guess)
;;  - Calls itself again with the better guess
;;  - Prints result when satisfied

(define (square-rt x)
  (square-rt-iter 1.0 x))

(define (square-rt-iter guess x)
;;  (if (good-enough? guess x)
  (if (better-good-enough? guess x)
      guess
      (square-rt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))

(square-rt 4)
(square-rt 2)

(square-rt 0.00004)

;; Ex 1.6: Why is 'if' a special form? Why not just use 'cond'?
;;         What happens when Alyssa attempts to use this to compute
;;         square roots? Explain.
;;
;; A: Applicative order dictates that recursive functions using 'cond' in this
;;    way would run out of memory (recursion must be fully expanded in this case)

;; Ex 1.7: good-enough? as shown fails with too small or too big values. Modify it
;;        to track the changes between guesses and use *that* as the cutoff

(define (guesses-relative-delta guess x)
  (/ (abs (- guess (improve guess x)))
     guess))

(define (better-good-enough? guess x)
  (< (guesses-relative-delta guess x) 0.00001))

;; Ex 1.8: Create a cube root function in the same manner, with the following
;;          approximation for the next guess:
;;
;;          (x / y^2) + 2y
;;          --------------
;;                3

(define (cube-rt x)
  (cube-rt-iter 1.0 x))

(define (cube-rt-iter guess x)
;;  (if (good-enough? guess x)
  (if (better-good-enough? guess x)
      guess
      (cube-rt-iter (improve guess x) x)))

(define (cube x)
  (* x x x))

(define (improve guess x)
  (/
   (+ (/ x (square guess))
      (* 2 guess))
   3))

(cube-rt 8)

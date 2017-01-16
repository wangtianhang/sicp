#lang racket

(define size 2)

(define (square x)(* x x))

(define (sum-of-squares x y) 
  (+ (square x) (square y)))

(define (ads x)
  (cond ((> x 0) x) 
        ((= x 0) 0)
        ((< x 0) (- x))
  )
)

;==========牛顿求根法====================
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
     guess
     (sqrt-iter (improve guess x) x)
  )
)

(define (improve guess x)
  (average guess (/ x guess ))
  )

(define (average x y) 
  (/ (+ x y ) 2)
  )

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
  )

(define (sqrtNew x)
  (sqrt-iter 1.0 x)
  )
;==============end====================

;===============阶乘======================
(define (factorial n)
  (fact-iter 1 1 n)
  )


;尾调用 所以不会有栈空间变大
(define (fact-iter product counter max-count)
 (if (> counter max-count)
  product
  (fact-iter (* counter product) (+ counter 1) max-count)
  )
)

;=================end======================

;=================斐波那契数列==================
;迭代 a<-a+b b<-a
(define (fib n)
  (fib-iter 1 0 n)
)

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))
      )
  )
;================end======================

;==============求幂运算==================
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((evenNew? n) (square(fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))
        )
  )

(define (evenNew? n)
  (= (remainder n 2) 0)
  )
;===========================

;==============最大公约数=============
(define (gcdNew a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))
      )
  )
;=================================

;===========素数判定============
(define (smallest-divisor n )
  (find-divisor 2)
  )

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))
        )
  )

(define (divides? a b)
  (= (remainder b a) 0)
  )

(define (prime? n)
  (= n (smallest-divisor n))
  )
;=================================

;=========高阶函数抽象============
(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b)
         )
      )
  )

(define (sum-cubes a b)
  (sum cube a inc b)
  )

;(define (identity x) x)

(define (sum-integers a b)
       (sum identity a inc b)
       )
;==============end=================
;===========求pi====================
(define (pi-sum a b)
   (define (pi-next x)
   (+ x 4))
   (define (pi-term x)
    (/ 1.0 ( * x (+ x 2))))
  (sum pi-term a pi-next b)
  )

(define (piNew)
  (* 8 (pi-sum 1 10000))
  )
;==============================
;==========求积分,需要raknet支持闭包=============
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
;============================
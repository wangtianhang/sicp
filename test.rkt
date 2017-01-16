;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
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
  (= (/ n 2) 0)
  )
;===========================
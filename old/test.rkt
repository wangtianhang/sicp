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
;==============end=============

;==============最大公约数=============
(define (gcdNew a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))
      )
  )
;===============end==================

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
;=============end====================

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
;=================end=============

;==========求积分,需要raknet支持闭包=============
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
;=============end===============

;==========过程作为返回值================
(define (average-damp f)
  (lambda (x) (average x (f x)))
  )
;===============end===============

;=============序对================
(define (make-rat n d) (cons n d))

(define (number x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (number x))
  (display "/")
  (display (denom x))
  )

;(define one-half (make-rat 1 2))
;(print-rat one-half)

;(define one-through-four (list 1 2 3 4))
;one-through-four

;===============end===============

;============列表====================
(define (list-ref items n)
  (if( = n 0)
     (car items)
     (list-ref (cdr items) (- n 1))
     )
  )

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))
      )
  )

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))
      )
  )

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))
      )
  )
;============end===============

;===========树============
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))
        )
  )

;============end=====================

;==========全排列======================

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))
      )
  )

(define (permutations s)
  (if (null? s)
      (list null)
      (flatmap(lambda(x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))
                    )) 
              s )
      )
  )

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence)
  )

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))
;==========end======================

;=========存储状态=================
(define new-withdraw
  (let ((balance 100))
  (lambda (amount)   
    (if(>= balance amount)
     (begin (set! balance (- balance amount)) balance)
     "Insufficient funds"
     )
    )
  )
  )

;(new-withdraw 25)
;(new-withdraw 100)
;==========end================
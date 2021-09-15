; ========= Task 2 =========
(define (func a b) ; процедура вывода чисел в порядке возростания/убывания
  (display a)
  (display " ")
  (cond ((> a b)
        (func (- a 1) b))
        ((< a b)
         (func (+ a 1) b))))

(func 7 3) ; вызов процедуры

; ========= Task 1 =========
(define (fact n) ; процедура нахождения факториала числа
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (comb n k) ; процедура нахождения кол-ва комбинаций
  (cond ((or (and (= n k) (> n 0)) (and (= k 0) (< n 0)))
         1)
         ((and (> n 0) (> n k))
          (/ (fact n) (* (fact k) (fact (- n k)))))
         (else 0)
         )
  )

;(comb 10 2) ; вызов процедуры
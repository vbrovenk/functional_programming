; ================ Zadanie 1 ================
; поиск индекса максимального элемента
(define (find-max-elem in-vector)
  (define max-index 0) ; первичное задание максимального элемента
  (define (max-iter index) 
    (cond ((and (< index (vector-length in-vector)) ; если не вышли за пределы размера вектора
                ; и найден больший элемент в векторе
                (> (vector-ref in-vector index) (vector-ref in-vector max-index)))
           (set! max-index index) ; устанавливается новый индекс максимального числа
           (max-iter (+ index 1)))
          ((< index (vector-length in-vector)) ; если не вышли за пределы размера вектора
           (max-iter (+ index 1))) ; переход к следующему числу
          (else
           max-index))) ; иначе вернуть найденный индекс
  
  (max-iter 1)) ; вызов рекурсии со 2 элемента

; поиск индекса минимального элемента
(define (find-min-elem in-vector)
  (define min-index 0) ; первичное задание максимального элемента
  (define (min-iter index) 
    (cond ((and (< index (vector-length in-vector)) ; если не вышли за пределы размера вектора
                ; и найден меньший элемент в векторе
                (< (vector-ref in-vector index) (vector-ref in-vector min-index)))
           (set! min-index index) ; устанавливается новый индекс минимального числа
           (min-iter (+ index 1)))
          ((< index (vector-length in-vector)) ; если не вышли за пределы размера вектора
           (min-iter (+ index 1))) ; переход к следующему числу
          (else
           min-index))) ; иначе вернуть найденный индекс
  
  (min-iter 1)) ; вызов рекурсии со 2 элемента

; вычисление суммы между минимальным и макимальным индексами
(define (sum-between min-index max-index in-vector)
  (define (sum-iter start-index end-index sum)
    (cond ((<= start-index end-index) ; пока не закончились элементы между start и end
           ; к сумме добавить элемент из вектора и перейти к следующему элементу
           (sum-iter (+ start-index 1) end-index (+ sum (vector-ref in-vector start-index))) )
          (else
           sum))) ; иначе вернуть сумму
  
  ; если min индекс меньше max индекса
  (if (< min-index max-index)
      (sum-iter min-index max-index 0) ; запуск рекурсии от min до max
      (sum-iter max-index min-index 0)); иначе запуск рекурсии от max до min
  )

; создание вектора
(define vector-nbrs (vector 1 20 1 4 -5 6))
(display "Вектор: ") (display vector-nbrs) (newline)

(define min-index (find-min-elem vector-nbrs)) ; поиск индекса минимального числа
(define min-number (vector-ref vector-nbrs min-index))
(display "Минимальное число: ") (display min-number)
(display " с индексом: ") (display min-index) (newline)

(define max-index (find-max-elem vector-nbrs)) ; поиск индекса максимального числа
(define max-number (vector-ref vector-nbrs max-index))
(display "Максимальное число: ") (display max-number)
(display " с индексом: ") (display max-index) (newline)

(display "Сумма между min и max индексами: ")
(sum-between min-index max-index vector-nbrs)

; ================ Zadanie 2 ================
; представление списка в виде пары: (голова, конец)
; получение указателя на начало очереди
(define (head-ptr queue)
  (car queue))

; получение указателя на конец очереди
(define (tail-ptr queue)
  (cdr queue))

; изменение указателя начала очереди
(define (set-head-ptr! queue elem)
  (set-car! queue elem))

; изменение указателя конца очереди
(define (set-tail-ptr! queue elem)
  (set-cdr! queue elem))

; очередь пустая, если указатель на начало пустой
(define (empty-queue? queue)
  (null? (head-ptr queue)))

; создание очереди - пустой
(define (create-queue)
  (cons (list) (list)))

; обращение к голове очереди
(define (head-queue queue)
  (if (empty-queue queue)
      (display "head-queue error: обращение к пустой очереди")
      (car (front-ptr queue))))

; добавление элемента в конец очереди
(define (push-queue! queue elem)
  (let ((new-pair (cons elem (list)))) ; новая пара: элемент, пустой список
    (cond ((empty-queue? queue) ; если очередь пустая, то
           (set-head-ptr! queue new-pair) ; и начало очереди
           (set-tail-ptr! queue new-pair)) ; и конец указывают на новый элемент
          (else ; в противном случае
           (set-cdr! (tail-ptr queue) new-pair) ; изменить последнюю пару очереди
           (set-tail-ptr! queue new-pair) ; перенаправление хвостового указателя на последний элемент
           ))))

; удаление элемента из начала очереди
(define (pop-queue! queue)
  (cond ((empty-queue? queue) ; если очереди пустая
         (display "pop error: обращение к пустой очереди"))
        (else
         ; иначе перенаправление головного указателя на второй элемент очереди
         (set-head-ptr! queue (cdr (head-ptr queue))) 
         )))

; определение кол-ва элементов в очереди
(define (size-queue queue)
  (define (iter-queue elems length)
    (cond ((null? elems) ; если элементов нет
           length) ; вернуть найденное кол-во элементов
          (else
           (iter-queue (cdr elems) (+ length 1))))) ; переход к cdr от списка элементов
  
  (iter-queue (head-ptr queue) 0))

; среднее арифметическое
(define (ariphmetic-mean queue)
  (define (iter-queue elems sum)
    (cond ((null? elems) ; если элементов нет
           (display sum) (display "/") (display (size-queue queue))
           (display " = ")
           (/ sum (size-queue queue))) ; вернуть сумму элементов деленное на кол-во
          (else
           (iter-queue (cdr elems) (+ sum (car elems)))))) ; переход к cdr от списка элементов
  
  (iter-queue (head-ptr queue) 0.0))

; среднее геометрическое
(define (geometric-mean queue)
  (define (iter-queue elems product)
    (cond ((null? elems) ; если элементов нет
           (display product) (display "^1/") (display (size-queue queue))
           (display " = ")
           (expt product (/ 1 (size-queue queue)))) ; вернуть произведение в степени 1/кол-во элементов
          (else
           (iter-queue (cdr elems) (* product (car elems)))))) ; переход к cdr от списка элементов
  
  (iter-queue (head-ptr queue) 1.0))

; создание очереди и добавление чисел
(define queue-nbrs (create-queue))
(push-queue! queue-nbrs 3)
(push-queue! queue-nbrs 5)
(push-queue! queue-nbrs 8)
(push-queue! queue-nbrs 11)
(push-queue! queue-nbrs 15)
(push-queue! queue-nbrs 21)

;(display "Очередь из чисел: ")
;(display (head-ptr queue-nbrs)) (newline)
;(display "Размер очереди: ")
;(display (size-queue queue-nbrs)) (newline)
;(display "Среднее арифметическое: ")
;(display (ariphmetic-mean queue-nbrs)) (newline)
;(display "Среднее геометрическое: ")
;(display (geometric-mean queue-nbrs)) (newline)


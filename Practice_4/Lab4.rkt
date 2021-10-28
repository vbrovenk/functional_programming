; ================ Zadanie 1 ====================
; создание списка от start с кол-вом n
(define (create-list start n creating-list)
    (set! creating-list (append creating-list (list start))) ; добавление к списке след. элемента
    (if (= (length creating-list) n) ; если длина списка равна заданному кол-ву элементов
        creating-list ; вернуть список
        (create-list (+ start 2 ) n creating-list) ; перейти к добавлению след. элемента
    )
)

; список четных чисел
(define even-list (list))
; создание списка четных чисел
(define (create-even-list count)
  (set! even-list (create-list 2 count (list))))

; добавление элементов к исходному списку
(define (append-list-to-start n)
  ; сначала создание нового списка, а затем присоединение к нему исходного
  (set! even-list (append
                   (create-list 2 n (list))
                   even-list
                   )
        )
  )

; b) поиск элемента в списке
(define (search-elem in-list element)
  (define (search-iter i tmp-list)
    ; если список не пуст и первый элемент списка равен заданному числу
    (cond ((and (not (null? tmp-list)) (= (car tmp-list) element)) 
           i) ; вернуть найденный индекс числа
          ((null? tmp-list) ; если список пуст
           (display "Заданного элемента нет в списке") (newline)) ; элемента нет в списке
          ((not (null? tmp-list)) ; пока список не пуст
           (search-iter (+ i 1) (cdr tmp-list)) ; перейти к поиску элемента в cdr(список)
          )))
  (search-iter 0 in-list)
  )
; с) кол-во элементов, значения которых в заданном диапазоне 
(define (count-elem-in-range a b list-in)
  (define sum 0)
  (define (sum-iter list-tmp)
    (cond ((null? list-tmp) ; если список пуст
           (display sum)) ; вывести кол-во элементов
          ; если первый элемент списка находится в заданном промежутке
          ((and (>= (car list-tmp) a) (<= (car list-tmp) b))
           (set! sum (+ sum 1)))) ; добавить к сумме элементов 1
    (if (not (null? list-tmp)) ; пока список не пуст
        (sum-iter (cdr list-tmp)))) ; выполнить переход к cdr(список)
          
  (sum-iter list-in)
  )

; создание списка четных чисел с указанием кол-ва элементов
(create-even-list 10) 
;(display "Исходный список четных чисел:\t\t\t")(display even-list)(newline)
; добавление элементов к началу списка с указанием кол-ва добавляемых элементов
;(append-list-to-start 2)
;(display "Список после добавления элементов в начало:\t")(display even-list)(newline)
;(display "Поиск элемента: ")(search-elem even-list 6)
;(display "Кол-во элементов в заданном диапазоне: ")(count-elem-in-range 3 14 even-list)

; ================ Zadanie 2 ====================
; вывод очередей процессов
(define (display-processes new-proc ready-proc)
  (display "Новые процессы: ") (display new-proc)
  (display " | Готовые процессы: ") (display ready-proc)
  (newline))
; обработка процесса CPU
(define (cpu process)
  (display "Выполняется обработка процесса: ") (display process) (newline))

; кол-во готовых процессов, которое может содержать основная память
(define size-memory 3)
; очередь готовых процессов в основной памяти
(define ready-processes (list 7 8 9))
; планировщик процессов
(define (scheduler)
  ; если кол-во готовых процессов меньше установленного, то можно добавить процесс из новых
  (cond ((and (< (length ready-processes) size-memory) (not (null? new-processes)))
         (display "Указатель -> Новые процессы: ") (display new-processes) (newline)
         (display "Взятие процесса из очереди новых и добавление к готовым: ")
         (display (car new-processes)) (newline)
         ; добавление к готовым процессам нового
         (set! ready-processes (append ready-processes (list(car new-processes))))
         ; удаление из новых процессов, перемещенного в готовые процессы
         (set! new-processes (cdr new-processes))
         (display-processes new-processes ready-processes)
         (display "=======================================") (newline)
         ))
  (cond ((not (null? ready-processes)) ; пока очередь готовых процессов не пустая
         (display "Указатель -> Готовые процессы: ") (display ready-processes) (newline)
         (cpu (car ready-processes)) ; передать выбранный процесс на обработку CPU
         (set! ready-processes (cdr ready-processes)) ; удалить выбранный процесс из очереди готовых
         (display-processes new-processes ready-processes)
         (display "=======================================") (newline)
         (scheduler)))
  )
          
; очередь новых процессов
(define new-processes (list 11 12))
(scheduler)

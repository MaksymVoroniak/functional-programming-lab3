<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Функціональний і імперативний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Вороняк Максим Ігорович КВ-22</p>
<p align="right"><b>Рік</b>: 2026</p>

## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і
імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного
списку. Не допускається використання: деструктивних операцій, циклів, функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Також реалізована функція не має
бути функціоналом (тобто приймати на вхід функції в якості аргументів).
2. Імперативний варіант реалізації має базуватись на використанні циклів і
деструктивних функцій (псевдофункцій). Не допускається використання функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Тим не менш, оригінальний список
цей варіант реалізації також не має змінювати, тому перед виконанням
деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
Також реалізована функція не має бути функціоналом (тобто приймати на вхід
функції в якості аргументів).
Алгоритм, який необхідно реалізувати, задається варіантом (п. 3.1.1). Зміст і шаблон
звіту наведені в п. 3.2.
Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів (наприклад, як наведено у п. 2.3).

## Варіант 3
Алгоритм сортування обміном №2 (із використанням прапорця) за незменшенням.

## Лістинг функції з використанням конструктивного підходу
```lisp
(defun bubble-pass-functional (lst)
  "Виконує один прохід бульбашки. 
   Повертає два значення: (новий-список чи-були-зміни)."
  (cond 
    ;; Базовий випадок: кінець списку або один елемент
    ((null (cdr lst)) (values lst nil))
    
    ;; Якщо поточний елемент більший за наступний -> міняємо місцями
    ((> (car lst) (cadr lst))
     (multiple-value-bind (sorted-tail changed)
         ;; Рекурсивний виклик для (car lst) і решти хвоста
         (bubble-pass-functional (cons (car lst) (cddr lst)))
       ;; Конструюємо список: другий елемент стає першим
       (values (cons (cadr lst) sorted-tail) t)))
    
    ;; Якщо порядок правильний -> йдемо далі
    (t
     (multiple-value-bind (sorted-tail changed)
         (bubble-pass-functional (cdr lst))
       (values (cons (car lst) sorted-tail) changed)))))

(defun functional-sort (lst)
  "Головна функція конструктивного сортування."
  (multiple-value-bind (new-list changed) (bubble-pass-functional lst)
    (if changed
        (functional-sort new-list) ; Якщо були зміни, ще один прохід
        new-list)))                ; Список відсортовано
```

### Тестові набори
```lisp
(defun check-functional-sort (name input expected)
  "Тестова утиліта для перевірки функції functional-sort"
  (let ((result (functional-sort input)))
    (format t "~:[FAILED~;passed~]... ~a~%  Input: ~a~%  Result: ~a~%"
            (equal result expected) ; Перевіряємо рівність списків
            name input result)))

(defun test-functional-sets ()
  (format t "~%--- Testing Functional Sort ---~%")
  
  ;; Тест 1: Звичайний невідсортований список
  (check-functional-sort "Normal list" 
                         '(3 1 4 1 5 9 2 6) 
                         '(1 1 2 3 4 5 6 9))
  
  ;; Тест 2: Список, відсортований у зворотному порядку
  (check-functional-sort "Reverse sorted" 
                         '(5 4 3 2 1) 
                         '(1 2 3 4 5))
  
  ;; Тест 3: Вже відсортований список (перевірка, що нічого не ламається)
  (check-functional-sort "Already sorted" 
                         '(1 2 3 4 5) 
                         '(1 2 3 4 5))
  
  ;; Тест 4: Список з дублікатами
  (check-functional-sort "With duplicates" 
                         '(2 2 1 1 3 3) 
                         '(1 1 2 2 3 3))
  
  ;; Тест 5: Порожній список (граничний випадок)
  (check-functional-sort "Empty list" 
                         '() 
                         nil)
  (format t "-------------------------------~%"))
```
### Тестування
```lisp
CL-USER> (test-functional-sets)

--- Testing Functional Sort ---
passed... Normal list
  Input: (3 1 4 1 5 9 2 6)
  Result: (1 1 2 3 4 5 6 9)
passed... Reverse sorted
  Input: (5 4 3 2 1)
  Result: (1 2 3 4 5)
passed... Already sorted
  Input: (1 2 3 4 5)
  Result: (1 2 3 4 5)
passed... With duplicates
  Input: (2 2 1 1 3 3)
  Result: (1 1 2 2 3 3)
passed... Empty list
  Input: NIL
  Result: NIL
-------------------------------
NIL
```
## Лістинг функції з використанням деструктивного підходу
```lisp
(defun imperative-sort (lst)
  "Сортування обміном з прапорцем (деструктивне).
   Використовує цикли та rotatef."
  (let ((result (copy-list lst)) ; Копіюємо список
        (swapped t))             ; Прапорець обмінів
    (loop while swapped do
      (setf swapped nil)
      ;; Проходимо по списку, отримуючи доступ до cons-комірок
      (loop for head on result while (cdr head) do
        (when (> (car head) (cadr head))
          ;; Міняємо значення в комірках місцями
          (rotatef (car head) (cadr head))
          (setf swapped t))))
    result))
```

### Тестові набори
```lisp
(defun check-sort (name input expected)
  "Перевіряє обидві реалізації сортування."
  (let ((func-res (functional-sort input))
        (imp-res (imperative-sort input)))
    (format t "~%Test ~a:~%Functional: ~:[FAILED~;passed~] result: ~a~%Imperative: ~:[FAILED~;passed~] result: ~a~%" 
            name 
            (equal func-res expected) func-res
            (equal imp-res expected) imp-res)))

(defun test-lab-3 ()
  (check-sort "Random list" '(3 1 4 1 5 9 2 6) '(1 1 2 3 4 5 6 9))
  (check-sort "Reverse sorted" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-sort "Already sorted" '(1 2 3) '(1 2 3))
  (check-sort "With duplicates" '(2 2 1 1 3) '(1 1 2 2 3))
  (check-sort "Empty" '() nil))
```

### Тестування
```lisp
CL-USER> (test-lab-3)

Test Random list:
Functional: passed result: (1 1 2 3 4 5 6 9)
Imperative: passed result: (1 1 2 3 4 5 6 9)

Test Reverse sorted:
Functional: passed result: (1 2 3 4 5)
Imperative: passed result: (1 2 3 4 5)

Test Already sorted:
Functional: passed result: (1 2 3)
Imperative: passed result: (1 2 3)

Test With duplicates:
Functional: passed result: (1 1 2 2 3)
Imperative: passed result: (1 1 2 2 3)

Test Empty:
Functional: passed result: NIL
Imperative: passed result: NIL
```
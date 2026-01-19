;;; Вступ до функціонального програмування
;;; Лабораторна робота 3
;;; Варіант 3

;;; ЧАСТИНА 1: Конструктивний підхід (Функціональний)
;;; Алгоритм: Сортування обміном (Bubble Sort) з прапорцем.


(defun bubble-pass-functional (lst)
  "Виконує один прохід по списку.
   Повертає values: (новий-список чи-відбувся-обмін)."
  (cond 
    ;; Якщо список закінчився або має 1 елемент - повертаємо як є
    ((null (cdr lst)) (values lst nil))
    
    ;; Якщо перший елемент більший за другий -> обмін
    ((> (car lst) (cadr lst))
     ;; Рекурсивно сортуємо "хвіст", де замість голови передаємо другий елемент
     (multiple-value-bind (sorted-tail changed)
         (bubble-pass-functional (cons (car lst) (cddr lst)))
       ;; Формуємо новий список: (cadr lst) йде першим
       ;; Прапорець changed ставимо в T
       (values (cons (cadr lst) sorted-tail) t)))
    
    ;; Якщо порядок правильний
    (t
     (multiple-value-bind (sorted-tail changed)
         (bubble-pass-functional (cdr lst))
       ;; Просто приєднуємо голову до відсортованого хвоста
       (values (cons (car lst) sorted-tail) changed)))))

(defun functional-sort (lst)
  "Головна функція сортування (конструктивна)."
  (multiple-value-bind (new-list changed) (bubble-pass-functional lst)
    (if changed
        (functional-sort new-list) ; Якщо були зміни, потрібен ще прохід
        new-list)))                ; Якщо змін не було, список відсортовано

;;; ЧАСТИНА 2: Деструктивний підхід (Імперативний)


(defun imperative-sort (lst)
  "Сортування обміном з прапорцем (деструктивне).
   Використовує copy-list для безпеки вхідних даних."
  (let ((result (copy-list lst)) ; 1. Копіюємо список
        (swapped t))             ; 2. Прапорець
    (loop while swapped do
      (setf swapped nil)
      ;; loop ... on ... дозволяє ітеруватись по комірках (cons-cells)
      (loop for head on result while (cdr head) do
        (when (> (car head) (cadr head))
          ;; Деструктивний обмін значень у комірках
          (rotatef (car head) (cadr head))
          (setf swapped t))))
    result))


;;; Тестування

(defun check-sort (name input expected)
  "Утиліта для перевірки обох функцій."
  (let ((func-res (functional-sort input))
        (imp-res (imperative-sort input)))
    (format t "~%Test ~a:~%Functional: ~:[FAILED~;passed~] result: ~a~%Imperative: ~:[FAILED~;passed~] result: ~a~%" 
            name 
            (equal func-res expected) func-res
            (equal imp-res expected) imp-res)))

(defun test-lab-3 ()
  (format t "--- START TESTING ---")
  (check-sort "Random list" 
              '(3 1 4 1 5 9 2 6) 
              '(1 1 2 3 4 5 6 9))
  
  (check-sort "Reverse sorted" 
              '(5 4 3 2 1) 
              '(1 2 3 4 5))
  
  (check-sort "Already sorted" 
              '(1 2 3 4 5) 
              '(1 2 3 4 5))
              
  (check-sort "Duplicates" 
              '(2 2 1 1 3 3) 
              '(1 1 2 2 3 3))
              
  (check-sort "Empty list" 
              '() 
              nil)
  (format t "~%--- END TESTING ---~%"))

;; Запуск тестів
(test-lab-3)
<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Поляков Єгор Олегович КВ-33</p>
<p align="right"><b>Рік</b>: 2026</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом (п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею. 
  1. Визначити структури та/або утиліти для створення записів з таблиць (в залежності від типу записів, заданого варіантом).
  2. Розробити утиліту(-и) для зчитування таблиць з файлів. Значення колонок мають бути розібрані відповідно до типу даних у них. Наприклад, рядок — це просто рядок; числові колонки необхідно розібрати як цілі числа або числа з рухомою крапкою.
  3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і т. і. За потреби параметрів може бути кілька.  select повертає лямбда-вираз, який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано у  select . При цьому лямбда-вираз в якості ключових параметрів може отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). Вибірка повертається у вигляді списку записів.
  4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
  5. Написати функції для конвертування записів у інший тип (в залежності від варіанту): 
      * структури у геш-таблиці 
      * геш-таблиці у асоціативні списки 
      * асоціативні списки у геш-таблиці
  6. Написати функцію(-ї) для "красивого" виводу записів таблиці (pretty-print)

## Варіант 12
**База даних:** Космічні апарати
**Тип Записів:** Асоціативний список
**Таблиці:**
  1. Компанії
  2. Космічні апарати
**Опис:** База даних космічних апаратів для зв'язку, дослідження, тощо


## Лістинг реалізації завдання

```lisp
(require :asdf)

(defparameter *companies-columns*
  '((:id :integer)
    (:name :string)
    (:country :string)
    (:founded :integer)))

(defparameter *spacecraft-columns*
  '((:id :integer)
    (:name :string)
    (:company-id :integer)
    (:launch-year :integer)
    (:mass-kg :float)))

(defun parse-value (value type)
  (case type
    (:integer (parse-integer value))
    (:float (float (read-from-string value)))
    (t value)))

(defun make-record (line columns)
  (mapcar (lambda (column value)
            (cons (first column) (parse-value value (second column))))
          columns
          (uiop:split-string line :separator '(#\,))))

(defun read-records (stream columns)
  (let ((line (read-line stream nil)))
    (if line
        (cons (make-record line columns)
              (read-records stream columns))
        nil)))

(defun read-table (path columns)
  (with-open-file (stream path)
    (read-line stream)
    (read-records stream columns)))

(defun select (path columns)
  (lambda (&rest filters &key &allow-other-keys)
    (remove-if-not (lambda (record)
                     (every (lambda (pair)
                              (equalp (cdr pair)
                                      (getf filters (car pair) (cdr pair))))
                            record))
                   (read-table path columns))))

(defun write-records (records path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (format stream "~{~a~^,~}~%" (mapcar #'car (first records)))
    (dolist (record records)
      (format stream "~{~a~^,~}~%" (mapcar #'cdr record)))))

(defun alist-to-hash-table (alist)
  (let ((table (make-hash-table)))
    (dolist (pair alist)
      (setf (gethash (car pair) table) (cdr pair)))
    table))

(defun pretty-print (records)
  (format t "~{~25a~}~%" (mapcar #'car (first records)))
  (dolist (record records)
    (format t "~{~25a~}~%" (mapcar #'cdr record))))

```

### Тестові набори та утиліти

```lisp
(defun check-test (name actual expected)
  (format t "~:[FAILED~;passed~]... ~a~%" (equal actual expected) name))

(defun test-lab5 ()
  (check-test "test 1"
              (make-record "1,SpaceX,USA,2002" *companies-columns*)
              '((:id . 1) (:name . "SpaceX") (:country . "USA") (:founded . 2002)))
  (check-test "test 2"
              (make-record "1,Crew Dragon,1,2020,12055" *spacecraft-columns*)
              '((:id . 1) (:name . "Crew Dragon") (:company-id . 1)
                (:launch-year . 2020) (:mass-kg . 12055.0)))
  (check-test "test 3"
              (length (read-table "companies.csv" *companies-columns*))
              5)
  (check-test "test 4"
              (length (read-table "spacecraft.csv" *spacecraft-columns*))
              7)
  (check-test "test 5"
              (length (funcall (select "companies.csv" *companies-columns*)))
              5)
  (check-test "test 6"
              (funcall (select "companies.csv" *companies-columns*) :country "USA")
              '(((:id . 1) (:name . "SpaceX") (:country . "USA") (:founded . 2002))
                ((:id . 2) (:name . "Blue Origin") (:country . "USA") (:founded . 2000))))
  (check-test "test 7"
              (funcall (select "spacecraft.csv" *spacecraft-columns*)
                       :company-id 1 :launch-year 2023)
              '(((:id . 2) (:name . "Starlink V2 Mini") (:company-id . 1)
                 (:launch-year . 2023) (:mass-kg . 800.0))))
  (check-test "test 8"
              (funcall (select "companies.csv" *companies-columns*) :country "Ukraine")
              nil)
  (check-test "test 9"
              (progn (write-records (read-table "companies.csv" *companies-columns*)
                                    "result.csv")
                     (read-table "result.csv" *companies-columns*))
              (read-table "companies.csv" *companies-columns*))
  (check-test "test 10"
              (gethash :name (alist-to-hash-table '((:id . 1) (:name . "SpaceX"))))
              "SpaceX")
  (check-test "test 11"
              (gethash :country (alist-to-hash-table '((:id . 1) (:name . "SpaceX"))))
              nil))
```

### Тестування

```lisp
(test-lab5)
(pretty-print (funcall (select "companies.csv" *companies-columns*)))
(pretty-print (funcall (select "spacecraft.csv" *spacecraft-columns*) :company-id 1))
```
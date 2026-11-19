;; Лістинг реалізації завдання
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

;; Тестові набори та утиліти
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

;; Тестування
(test-lab5)
(pretty-print (funcall (select "companies.csv" *companies-columns*)))
(pretty-print (funcall (select "spacecraft.csv" *spacecraft-columns*) :company-id 1))

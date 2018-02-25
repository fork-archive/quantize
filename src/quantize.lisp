;;(in-package #:quantize)

;; a data file is a list of datepackets
;; a datepacket is a plist with a :date field and a :chunks field
;; a chunk is a plist with a :duration field and a :tags field

(defparameter *raw-data* nil)
(defparameter *logs* nil)
(defparameter *tags* nil)
(defparameter *datepacket* nil)
(defparameter *data-file-version* '(0 0))
(defparameter *today* nil)

(defun uplift-data (data)
  (let* ((version (getf data :version))
         (major (nth 0 version))
         (minor (nth 1 version))
         (mm (list major minor)))
    (cond
      ((equal mm *data-file-version*)
       data)
      ((equal mm '(0 0))
       (format t "error! UPLIFT-DATA not implemented for version ~s~%" mm))
      (t
       (format t "error! invalid file version: ~s~%" version)))))

(defun import-data-file (&optional path)
  (let ((p (or path (resource "fouric.dat" 'quantize))))
    (let ((raw-data (uplift-data (fouriclib:read-file p))))
      (setf *logs* (getf raw-data :data)
            *tags* (getf raw-data :tags)))))

(defun export-data-file (&optional path)
  (let ((p (or path (resource "fouric.dat" 'quantize))))
    (fouriclib:write-file p (list :version '(0 0) :data *logs* :tags *tags*))))

(defun select-date (&rest values)
  ;; TODO: what the heck
  ;; document pls
  (let* ((previous-date (getf (first *logs*) :date))
         (year (nth 2 (reverse (if (<= 3 (length values)) values previous-date))))
         (month (nth 1 (reverse (if (<= 2 (length values)) values previous-date))))
         (day (nth 0 (reverse (if (<= 1 (length values)) values previous-date))))
         (date (list year month day))
         datepacket)
    ;; wat
    (dolist (dp *logs*)
      (when (equal (getf dp :date) date)
        (setf datepacket dp)
        (return)))
    ;; wat
    (unless datepacket
      (setf datepacket (list :date (list year month day) :chunks ()))
      (push datepacket *logs*))
    (setf *datepacket* datepacket
          *today* nil)))

(defun get-time-contribution (chunk &optional (tag 'school-requires))
  (append (list tag) (flat-get-recursive-tag-parents (getf chunk :tags)))
  (if (member (append (flat-get-recursive-tag-parents (getf chunk :tags)))
              (case category
                (school
                 '(ph319 ece321 ece321q ece331 ece371 studying))
                (transit
                 '(bus-walking bus-riding transit-to-school))
                (personal
                 '(toodling coding))
                (hygiene
                 '(hygiene sleeping))))
      (getf chunk :duration)
      0))

(defun get-tag-time (tag)
  (let ((time 0))
    (dolist (date *logs*)
      (let ((chunks (getf date :chunks)))
        (dolist (chunk chunks)
          (let ((tags (append (list tag) (flat-get-recursive-tag-parents tag))))
            (when (member tag tags)
              (incf time (getf chunk :duration)))))))
    time))

(defun print-as-hm (time label)
  (format t (concatenate 'string label "~a (~ah ~am)~%") time (floor (/ time 60)) (mod time 60)))

(defun get-total-time-contribution (&optional (category 'school))
  (let ((time (apply #'+ (mapcar (lambda (c) (get-time-contribution c category)) (getf *datepacket* :chunks)))))
    (print-as-hm time "~%total: ")))

(defun get-max-index ()
  (let ((chunks (getf *datepacket* :chunks)))
    (apply #'max (append (list -1) (mapcar (lambda (dp) (getf dp :index)) chunks)))))

(defun add-chunk (duration tags)
  (let ((max-index (get-max-index)))
    (push (list :index (1+ max-index) :duration duration :tags tags) (getf *datepacket* :chunks))))

(defun print-datepacket ()
  (let ((date (getf *datepacket* :date)))
    (format t "~a-~a-~a~%~%" (nth 0 date) (nth 1 date) (nth 2 date)))
  (let ((total 0))
    (dolist (chunk (sort (copy-list (getf *datepacket* :chunks)) (lambda (a b) (< (getf a :index) (getf b :index)))))
      (let ((duration (getf chunk :duration)))
        (format t "~a  |  ~a~%" duration (getf chunk :tags))
        (incf total duration)))
    (print-as-hm total "~%total: ")))

(defun print-dates ()
  (dolist (day *logs*)
    (format t "~s~%" (getf day :date))))

(defun help ()
  (print '(import-data-file (&optional path)))
  (print '(export-data-file (&optional path)))
  (print '(select-date (year month day)))
  (print '(add-chunk (duration tags)))
  (print '(print-datepacket))
  (print '(print-dates))
  (print '(get-time-contribution (chunk &optional (category 'school))))
  (print '(get-total-time-contribution (&optional (category 'school))))
  nil)

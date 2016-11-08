;;(in-package #:quantize)

;; a data file is a list of datepackets
;; a datepacket is a plist with a :date field and a :chunks field
;; a chunk is a plist with a :duration field and a :description field

(defparameter *file* nil)
(defparameter *datepacket* nil)

(defun read-data-file (&optional path)
  (let ((p (or path (resource "fouric.dat" 'quantize))))
    (setf *file* (read-file p))))

(defun select-date (year month day)
  (let ((date (list year month day))
	datepacket)
    (dolist (dp *file*)
      (when (equal (getf dp :date) date)
	(setf datepacket dp)
	(return)))
    (setf *datepacket* datepacket)))

(defun get-max-index ()
  (let ((chunks (getf *datepacket* :chunks)))
    (apply #'max (mapcar (lambda (dp) (getf dp :index)) chunks))))

(defun add-chunk (duration description)
  (let ((max-index (get-max-index)))
    (push (list :index (1+ max-index) :duration duration :description description) (getf *datepacket* :chunks))))

(defun write-data-file (&optional path)
  (let ((p (or path (resource "fouric.dat" 'quantize))))
    (write-file p *file*)))

(defun print-datepacket ()
  (let ((date (getf *datepacket* :date)))
    (format t "~a-~a-~a~%~%" (nth 0 date) (nth 1 date) (nth 2 date)))
  (let ((total 0))
    (dolist (chunk (sort (copy-list (getf *datepacket* :chunks)) (lambda (a b) (< (getf a :index) (getf b :index)))))
      (let ((duration (getf chunk :duration)))
	(format t "~a  |  ~a~%" duration (getf chunk :description))
	(incf total duration)))
    (format t "~%total: ~a~%" total)))

(defun help ()
  (print '(read-data-file (&optional path)))
  (print '(select-date (year month day)))
  (print '(add-chunk (duration description)))
  (print '(write-data-file))
  (print '(print-datepacket)))

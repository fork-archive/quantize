;;(in-package #:quantize)

;; a data file is a list of datepackets
;; a datepacket is a plist with a :date field and a :chunks field
;; a chunk is a plist with a :duration field and a :tags field

(defparameter *raw-data* nil)
(defparameter *logs* nil)
(defparameter *tags* nil)
;; *datepacket* holds the datepacket currently being operated on - like the cwd
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

(defun import-data (&optional (auto-load-last-date t) path)
  (let ((p (or path (fouriclib:resource "fouric.dat" 'quantize))))
    (let ((raw-data (uplift-data (fouriclib:read-file p))))
      (setf *logs* (getf raw-data :data)
            *tags* (getf raw-data :tags))
      (when auto-load-last-date
        (select-date)))))

(defun export-data (&optional path)
  (let ((p (or path (fouriclib:resource "fouric.dat" 'quantize))))
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
    ;; a datepacket is the plist for a day, including date and chunks
    ;; find a datepacket matching the given date
    (dolist (dp *logs*)
      (when (equal (getf dp :date) date)
        (setf datepacket dp)
        (return)))
    ;; OR, if it doesn't exist, create it
    (unless datepacket
      (setf datepacket (list :date (list year month day) :chunks ()))
      (push datepacket *logs*))
    ;; after either finding or creating, set the *datepacket* variable for functions that take it implicitly
    (setf *datepacket* datepacket)))

;; sketchy - will double-count time if you use it for related tags
(defun get-tag-time (tag)
  (let ((time 0))
    (dolist (date *logs*)
      (let ((chunks (getf date :chunks)))
        (dolist (chunk chunks)
          (let ((tags (remove-duplicates (append (getf chunk :tags) (apply #'append (mapcar #'flat-get-recursive-tag-parents (getf chunk :tags)))))))
            (when (member tag tags)
              (incf time (getf chunk :duration)))))))
    time))

(defun tag-time (tag)
  (get-tag-time tag))

(defun print-as-hm (time label)
  (format t (concatenate 'string label "~a (~ah ~am)~%") time (floor (/ time 60)) (mod time 60)))

(defun get-max-index ()
  (let ((chunks (getf *datepacket* :chunks)))
    (apply #'max (append (list -1) (mapcar (lambda (dp) (getf dp :index)) chunks)))))

(defun add-chunk (duration tags)
  (let ((max-index (get-max-index)))
    (push (list :index (1+ max-index) :duration duration :tags (if (listp tags) tags (list tags))) (getf *datepacket* :chunks))))

(defun tlog (duration tags)
  (add-chunk duration tags))

(defun add (duration tags)
  (add-chunk duration tags))

(defun print-datepacket ()
  (let ((date (getf *datepacket* :date)))
    (format t "~a-~a-~a~%~%" (nth 0 date) (nth 1 date) (nth 2 date)))
  (let ((total 0))
    (format t "CHUNKS~%~%")
    (dolist (chunk (sort (copy-list (getf *datepacket* :chunks)) (lambda (a b) (< (getf a :index) (getf b :index)))))
      (let ((duration (getf chunk :duration)))
        (format t "~4,' d  |  ~a~%" duration (getf chunk :tags))
        (incf total duration)))
    (print-as-hm total "~%total: "))
  (format t "~%TAGS~%~%")
  (dolist (tag *tags*)
    (let ((tag-name (getf tag :name)))
      (let ((time (reduce #'+ (mapcar (lambda (chunk) (getf chunk :duration)) (get-chunks-tagged tag-name)))))
        (when (plusp time)
          (format t "~4,' d  |  ~a~%" time tag-name))))))

(defun get-chunks-tagged (tag)
  (let* ((tag (ensure-tag-name tag))
         (children (append (list tag) (flat-get-recursive-tag-children tag)))
         chunks)
    (dolist (chunk (getf *datepacket* :chunks))
      (unless (= (+ (length children) (length (getf chunk :tags))) (length (remove-duplicates (append children(getf chunk :tags)))))
        (push chunk chunks)))
    chunks))

(defun get-chunks-tagged-intersection (tags)
  (reduce (lambda (x y) (intersection x y :test #'equal)) (mapcar #'get-chunks-tagged tags)))

(defun show ()
  (print-datepacket))

(defun major-times ()
  (dolist (tag '(life school-requires school noncrunch))
    (format t "~4,' d  |  ~a~%" (tag-time tag) (getf (ensure-tag-list tag) :name))))

(defun print-dates ()
  (dolist (day *logs*)
    (format t "~s~%" (getf day :date))))

#++(defun print-chunks ()
  (dolist (chunk (reverse (getf *datepacket* :chunks)))
    (format t "~4,' d  |  ~a~%" (getf chunk :duration) (getf chunk :name))))

(defun pop-chunk ()
  (pop (getf *datepacket* :chunks)))

(defun help ()
  (print '(import-data (&optional (auto-load-last-date t) path)))
  (print '(export-data (&optional path)))
  (print '(select-date (year month day)))
  (print '(add (duration tags)))
  (print '(show))
  (print '(print-dates))
  (print '(tag-time tag))
  (print '(major-times))
  (print '(pop-chunk))
  nil)

(defun enable-linedit-export-quit ()
  (setf (symbol-function (intern "EOF-HANDLER" :linedit))
            (lambda (lisp-name quit-fn)
              (declare (ignore lisp-name))
              (handler-case
                  (progn
                    (export-data)
                    (fresh-line)
                    (funcall quit-fn))
                (end-of-file ()
                  (fresh-line)
                  (funcall quit-fn))))))

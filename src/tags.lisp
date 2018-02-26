(defun create-tag (name &optional parents children)
  (let ((parents (if (listp parents) parents (list parents)))
        (children (if (listp children) children (list children))))
    (cond
      (children
       (create-tag name parents)
       (dolist (child children)
         (parent-tag name child)))
      (parents
       (create-tag name)
       (dolist (parent parents)
         (parent-tag parent name)))
      (t
       (push (list :name name :parents nil :children nil) *tags*)))))

(defun parent-tag (parent child)
  (let ((parent (ensure-tag-list parent))
        (child (ensure-tag-list child)))
    (push (getf parent :name) (getf child :parents))
    (push (getf child :name) (getf parent :children))))

(defun ensure-tag-list (tag)
  (etypecase tag
    (symbol
     (first (member tag *tags* :key (lambda (x) (getf x :name)))))
    (list
     tag)))

(defun tag-name (tag)
  (let ((tag (ensure-tag-list tag)))
    (getf tag :name)))

(defun ensure-tag-name (tag)
  (etypecase tag
    (symbol
     tag)
    (list
     (getf tag :name))))

(defun flat-get-recursive-tag-children (parent)
  (labels ((rec (tag &optional r)
             (append (let ((tag (ensure-tag-list tag)))
                       (apply #'append (mapcar #'rec (getf tag :children)))) (unless r (list (tag-name tag))))))
    (rec parent t)))

(defun flat-get-recursive-tag-parents (child)
  (labels ((rec (tag &optional r)
             (append (let ((tag (ensure-tag-list tag)))
                       (apply #'append (mapcar #'rec (getf tag :parents)))) (unless r (list (tag-name tag))))))
    (rec child t)))

#++(defun get-bookmark-all-applicable-tags (bookmark)
     (let ((bookmark (deref-bookmark bookmark)))
       (remove-duplicates (apply #'append (getf bookmark :tags) (mapcar #'flat-get-recursive-tag-parents (getf bookmark :tags))) :test #'string=)))

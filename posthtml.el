

(defmacro posthtml-filter-final-output (&rest body)
  `(lambda () (posthtml-filter-final-output* (posthtml ,@body))))


(defun posthtml-filter-final-output* (fn)
  (add-to-list 'org-export-filter-final-output-functions (posthtml* fn)))


(defmacro posthtml (&rest body)
  `(lambda (contents &optional info) ,@body))


(defun posthtml* (fn)
  (lambda (contents backend info) (funcall fn contents info)))


(defun posthtml/doctype ()
  (setf contents (concatenate 'string "<!DOCTYPE html>\n" contents)))

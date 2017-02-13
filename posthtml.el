
(require 'enlive)


(defvar posthtml-doctype)

(defmacro posthtml-filter-final-output (&rest body)
  `(lambda () (posthtml-filter-final-output* (posthtml ,@body))))

(defmacro posthtml (&rest body)
  `(lambda (contents &optional info)
     (when (not (listp contents))
       (setf contents (enlive-parse contents)))
     ,@body
     (when (listp contents)
       (setf contents (esxml-to-xml contents)))
     (when posthtml-doctype
       (setf contents (concatenate 'string posthtml-doctype "\n" contents)))
     contents))


(defun posthtml-filter-final-output* (fn)
  (add-to-list 'org-export-filter-final-output-functions (posthtml* fn)))

(defun posthtml* (fn)
  (lambda (contents backend info) (funcall fn (enlive-parse contents) info)))

(defun posthtml/doctype (&optional doctype)
  (setf posthtml-doctype (or doctype "<!DOCTYPE html>")))

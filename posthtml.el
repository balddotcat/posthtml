
(require 'esxml)
(require 'enlive)

(defvar posthtml-doctype '())


(defmacro posthtml-filter-final-output (&rest body)
  `(lambda () (posthtml-filter-final-output* (posthtml ,@body))))

(defun posthtml-filter-final-output* (fn)
  (add-to-list 'org-export-filter-final-output-functions (posthtml* fn)))


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

(defun posthtml* (fn)
  (lambda (contents backend info) (funcall fn (enlive-parse contents) info)))


(defun posthtml-append (container element)
  (if (= 1 (length container))
      (nconc container (list '() element))
    (nconc container (list element))))

(defun posthtml-query (&optional contents selector)
  (if (> 1 (length selector)) '()
    (or (enlive-query contents selector)
        (let ((selectors (append selector nil)))
          (while selectors
            (let ((container (enlive-query contents (vconcat (list (car selectors))))))
              (if (null container) (posthtml-append contents `(,(car selectors)))
                (progn (setf contents container)
                       (setf selectors (cdr selectors))))))
          contents))))


(defmacro def-posthtml/element (selector)
  (let ((name (mapconcat (lambda (s) (format "%s" s)) (cdr (append selector nil)) "-")))
    `(defun ,(intern (concat "posthtml/" name)) (&optional content)
       (posthtml$ ,selector content))))

(defun posthtml$ (selector &optional content)
  (let ((element (posthtml-query contents selector)))
    (when (and element content) (posthtml-append element content))
    element))


(defun posthtml/doctype (&optional doctype)
  (setf posthtml-doctype (or doctype "<!DOCTYPE html>")))

(def-posthtml/element [html head title])

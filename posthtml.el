
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
    (nconc container (list element)))
  container)

(defun posthtml-prepend (container element)
  (if (> 3 (length container))
      (posthtml-append container element)
    (setf (nthcdr 2 container) (append (list element) (nthcdr 2 container))))
  container)

(defun posthtml-find (elements &optional selector)
  (when selector
    (or (enlive-query elements selector)
        (let* ((selector (append selector nil))
               (prepend nil)
               (element (let ((element (split-string (format "%s" (car selector)) ":" t)))
                          (when (< 1 (length element))
                            (cond ((string= "first" (cadr element)) (setf prepend t))))
                          (intern (car element))))
               (container (enlive-query elements (vconcat (list element)))))
          (if container (or (posthtml-find container (vconcat (cdr selector))) container)
            (posthtml-find (if prepend (posthtml-prepend elements (list element))
                             (posthtml-append elements (list element)))
                           (vconcat selector)))))))

(defun posthtml$ (selector &optional content)
  (let ((element (posthtml-find contents selector)))
    (when (and element content) (posthtml-append element content))
    element))


(defmacro def-posthtml/element (selector)
  (let ((name (mapconcat (lambda (s) (format "%s" s)) (cdr (append selector nil)) "-")))
    `(defun ,(intern (concat "posthtml/" name)) (&optional content)
       (posthtml$ ,selector content))))


(defun posthtml/doctype (&optional doctype)
  (setf posthtml-doctype (or doctype "<!DOCTYPE html>")))

(def-posthtml/element [html head title])

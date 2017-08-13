;;; posthtml.el --- post HTML rendering filters for org-export
;; Copyright (C) Elo Laszlo 2017

;; Author: Elo Laszlo <hello at bald dot cat>
;; Created: August 2016
;; Updated: Aug 2017
;; Description: post HTML rendering filters for org-export
;; Homepage: http://bald.cat/project/posthtml
;; Version: 0.3.0
;; Package-Requires: ((esxml "20160703.1417")(enlive "20150824.549"))
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'esxml)                        ; esxml-to-xml
(require 'enlive)                       ; enlive-query, enlive-query-all
(eval-when-compile (require 'cl))       ; lexical-let, cl-macrolet
(require 'ox-html)                      ; org-html-doctype


;;;###autoload
(defmacro posthtml (&rest body)
  "An org-export-filter-final-output-functions definition with
the contents of BODY. CONTENTS and INFO are lexically bound.

CONTENTS is an esxml representation of the DOM tree, as parsed
by libxml-parse-html-region; the function returns a string via
esxml-to-xml.

INFO is the org-export property plist. The doctype, when
specified, is derived from INFO.

Within BODY, the following shorthand is available;
=($ [selector] &optional fn args)= for
=(posthtml$ contents [selector] &optional fn args)=,
=($each [selector] &optional fn args)= for
=(posthtml$each contents [selector] &optional fn args)=."
  (let ((contents (make-symbol "contents"))
        (backend (make-symbol "backend"))
        (info (make-symbol "info")))
    `(function
      (lambda (,contents &optional ,backend ,info)
        (posthtml-render
         (lexical-let
             ((contents (if (not (listp ,contents))
                            (posthtml-parse-html ,contents)
                          ,contents))
              (info ,info))
           ;; be aware of macro binding when fn is a lambda
           (cl-macrolet
               (($ (selector &optional fn &rest args)
                   `(funcall 'posthtml$ contents ,selector ,fn ,@args))
                ($each (selector &optional fn &rest args)
                       `(funcall 'posthtml$each contents ,selector ,fn ,@args)))
             ,@body)
           contents)
         (and (and (not (listp ,contents)) ,info)
              (string-prefix-p "<!DOCTYPE" ,contents t)
              (org-html-doctype ,info)))))))


;;;###autoload
(defmacro posthtml-filter-export-output (&rest body)
  "add a posthtml filter to the default export process"
  `(add-to-list 'org-export-filter-final-output-functions (posthtml ,@body)))

;;;###autoload
(defmacro posthtml-add-export-filter (&rest body)
  "add a posthtml filter to the default export process,
returns an org export filter lambda"
  `(lambda (&optional project-properties) (posthtml-filter-export-output ,@body)))


(defun posthtml-render (contents &optional doctype)
  "return DOCTYPE and CONTENTS as a string"
  (format "%s%s"
          (if doctype (concat doctype "\n") "")
          (if (listp contents)
              (posthtml-parse-esxml contents)
            contents)))

(defvar posthtml-special-chars '("amp" "gt" "lt"))

(defun posthtml-parse-html (contents)
  (with-current-buffer (get-buffer-create " *posthtml*")
    (erase-buffer)
    (insert contents)
    (loop for c in posthtml-special-chars do
          (goto-char 1)
          (while (search-forward (concat "&" c ";") nil t)
            (replace-match (concat "%%" c "%%") nil t)))
    (libxml-parse-html-region (point-min) (point-max))))

(defun posthtml-parse-esxml (contents)
  (with-current-buffer (get-buffer-create " *posthtml*")
    (erase-buffer)
    (insert (esxml-to-xml contents))
    (loop for c in posthtml-special-chars do
          (goto-char 1)
          (while (search-forward (concat "%%" c "%%") nil t)
            (replace-match (concat "&" c ";") nil t)))
    (goto-char 1)
    (while (search-forward "<comment>" nil t) (replace-match "<!--" nil t))
    (goto-char 1)
    (while (search-forward "</comment>" nil t) (replace-match "-->" nil t))
    (buffer-string)))


(defvar posthtml-fn-tokens
  '(:append posthtml-append :prepend posthtml-prepend :attr posthtml-attribute))

(defun posthtml$each (container selector &optional fn &rest args)
  "Query CONTAINER for SELECTOR; apply each found element and
ARGS to FN, return results.

CONTAINER is an esxml list, SELECTOR is a vector; ie [html body].

The following shorthands are available for FN;
:append for 'posthtml-append
:prepend for 'posthtml-prepend
:attr for 'posthtml-attribute"
  (if fn
      (setq fn (or (plist-get posthtml-fn-tokens fn) fn))
    (setq fn 'identity))
  (mapcar (lambda (element) (apply fn element args))
          (enlive-query-all container selector)))

(defun posthtml$ (container selector &optional fn &rest args)
  "Query CONTAINER for SELECTOR; apply found element and ARGS
to FN, return results.

CONTAINER is an esxml list, SELECTOR is a vector; ie [html body].

The following shorthands are available for FN;
:append for 'posthtml-append
:prepend for 'posthtml-prepend
:attr for 'posthtml-attribute"
  (if fn
      (setq fn (or (plist-get posthtml-fn-tokens fn) fn))
    (setq fn 'identity))
  (apply fn (enlive-query container selector) args))


(defun posthtml-append (container &optional element)
  "Append ELEMENT to CONTAINER.

CONTAINER is an esxml list, ELEMENT is a list or a string."
  (when element
    (if (= 1 (length container))
        (nconc container (list '() element))
      (nconc container (list element)))))

(defun posthtml-prepend (container &optional element)
  "Prepend ELEMENT to CONTAINER.

CONTAINER is an esxml list, ELEMENT is a list or a string."
  (when element
    (if (> 3 (length container))
        (posthtml-append container element)
      (setf (nthcdr 2 container)
            (append (list element) (nthcdr 2 container))))
    container))

(defun posthtml-attribute (element attribute &optional values)
  "Return ELEMENT ATTRIBUTE; with VALUES argument, add ATTRIBUTE
with VALUE to ELEMENT.

ELEMENT is an esxml list, ATTRIBUTE and VALUES are strings."
  (let ((attributes (nth 1 element)))
    (when values
      (let ((new-attribute (make-symbol (format "%s" attribute)))
            (value (format "%s" values)))
        (cond ((null attributes)
               (setf (nth 1 element)
                     (list (cons new-attribute value))))
              ((listp attributes)
               (let ((current-attribute (assoc attribute (nth 1 element))))
                 (if current-attribute
                     (setcdr current-attribute
                             (format "%s %s" (cdr current-attribute) value))
                   (setf (nth 1 element)
                         (nconc (nth 1 element)
                                (list (cons new-attribute value))))))))))
    (and (not (null attributes))
         (listp attributes)
         (cdr (assoc attribute (nth 1 element))))))


(provide 'posthtml)
;;; posthtml.el ends here

;;; posthtml.el --- decorate HTML string DOM with CSS-like selectors -*- lexical-binding: t; -*-
;; Copyright (C) 2018 Elo Laszlo

;; Author: Elo Laszlo <hello at bald dot cat>
;; Created: December 2018
;; Package-Version: 0.5.4
;; Keywords: files
;; Homepage: http://bald.cat/posthtml
;; Package-Requires: ((emacs "25.3.1") (esxml "20171129.807") (enlive "20170725.1417"))
;; This file is not part of GNU Emacs

;;; License:
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
;; posthtml provides a fluent interface to decorating a string of HTML like
;; "<ul><li/><li/></ul>" - to add attributes such as class or id to elements,
;; or to do custom processing.
;;
;;     (posthtml-decorate "<ul><li/><li/></ul>"
;;                        '($ [ul] :attr :id 'CONTAINER)
;;                        '($each [ul li] :attr :class 'ELEMENT))
;;
;; The string is parsed into an esxml representation of the DOM tree by
;; libxml-parse-html-region - the defined decorators are applied, and a string
;; rendered via esxml-to-xml is returned.  Querying is done with enlive.
;;
;; As a typical use case scenario, org-export-filter-final-output-functions provide
;; access to the final output of any org-export, or org-publishing sequence; this
;; list consists of the last functions called with the rendered output, their output
;; being what's written to disk.
;;
;;     (add-to-list 'org-export-filter-final-output-functions
;;                  (lambda (contents backend info)
;;                    (posthtml-decorate contents
;;                                       '($ [body] :attr :style "background-color: black;"))))
;;

;;; Code:
(require 'esxml)                        ; esxml-to-xml
(require 'enlive)                       ; enlive-query, enlive-query-all

;;;###autoload
(defun posthtml-decorate (contents &rest decorators)
  "Decorate CONTENTS with each of DECORATORS.

CONTENTS is a string containing an HTML structure, which is
converted into an `esxml' representation of the DOM tree, as
parsed by `libxml-parse-html-region'; returns edited string via
`esxml-to-xml'.

The car of each decorator is a function called with an `esxml'
parse-tree, and the decorator's cdr as args when provided."
  (let* ((html-string (substring-no-properties contents))
         (esxml-parse-tree (posthtml--html-to-esxml html-string)))
    (concat (save-match-data
              (if (eq nil (string-match "^\\(<!DOCTYPE[^>]+?>\n?\\)" html-string)) ""
                (match-string 1 html-string)))
            (posthtml--esxml-to-html
             (let ((results (seq-reduce (lambda (contents decorator)
                                          (apply (car decorator) contents (cdr decorator)))
                                        (seq-filter 'posthtml--decorator decorators)
                                        esxml-parse-tree)))
               (if (eq nil (string-match "<html" html-string))
                   (if (eq nil (string-match "<body" html-string))
                       (car (cddr (enlive-query results [body])))
                     (enlive-query results [body]))
                 results))))))

(defun posthtml--decorator (&optional param)
  "Translates custom PARAM tokens to functions.

Please see `posthtml-decorate'."
  (when param
    (let* ((decorator (or (and (listp param) param)
                          (list param)))
           (action (cddr decorator)))
      (pcase (car decorator)
        ('$ (setf (car decorator) 'posthtml-apply))
        ('$each (setf (car decorator) 'posthtml-apply-each)))
      (when action
        (pcase (car action)
          (:append (setf (nth 2 decorator) 'posthtml-append))
          (:prepend (setf (nth 2 decorator) 'posthtml-prepend))
          (:attr (setf (nth 2 decorator) 'posthtml-attribute))
          (:attr-set (setf (nth 2 decorator) (lambda (element &rest attributes) (apply 'posthtml-attributes-set element attributes))))
          (:attrs (setf (nth 2 decorator) 'posthtml-attributes))
          (:attrs-set (setf (nth 2 decorator) 'posthtml-attributes-set))))
      decorator)))

(defun posthtml-apply (content selector fn &rest args)
  "Within CONTENT find element with SELECTOR, apply it to FN with ARGS.

CONTENT is an `esxml' list.  SELECTOR is a vector, as used by
`enlive'.  ARGS are `eval'-d in the current environment."
  (let ((esxml-parse-tree (posthtml-find content selector)))
    (setf esxml-parse-tree (apply fn esxml-parse-tree (mapcar 'eval args)))
    content))

(defun posthtml-apply-each (content selector fn &rest args)
  "Within CONTENT find each element with SELECTOR, apply it to FN with ARGS.

CONTENT is an esxml list.  SELECTOR is a vector, as used by
`enlive'.  ARGS are `eval'-d in the current environment."
  (dolist (esxml-parse-tree (posthtml-find-all content selector) content)
    (setf esxml-parse-tree (apply fn esxml-parse-tree (mapcar 'eval args)))))

(defun posthtml--html-to-esxml (contents)
  "Translate CONTENTS, a string of HTML to an `esxml' parse-tree."
  (with-temp-buffer
    (insert (substring-no-properties contents))
    (goto-char (point-min))
    (loop for token in
          '(("&amp;" . "%%amp%%")
            ("&lt;" . "%%lt%%")
            ("&gt;" . "%%gt%%"))
          do
          (save-excursion
            (while (search-forward (car token) nil t)
              (replace-match (cdr token) nil t))))
    (libxml-parse-html-region (point-min) (point-max))))

(defun posthtml--esxml-to-html (contents)
  "Translate CONTENTS, an `esxml' parse-tree, to an HTML string."
  (with-temp-buffer
    (insert (esxml-to-xml contents))
    (goto-char (point-min))
    (loop for token in
          '(("&amp;" . "%%amp%%")
            ("&lt;" . "%%lt%%")
            ("&gt;" . "%%gt%%"))
          do
          (save-excursion
            (while (search-forward (cdr token) nil t)
              (replace-match (car token) nil t))))
    (save-excursion
      (while (search-forward "<comment>" nil t)
        (replace-match "<!--" nil t)))
    (save-excursion
      (while (search-forward "</comment>" nil t)
        (replace-match "-->" nil t)))
    (loop for element in
          '("figure" "script" "a" "style")
          do
          (save-excursion
            (while (search-forward-regexp (format "<\\(%s\\).*?\\(/>\\)" element) nil t)
              (replace-match "></\\1>" nil nil nil 2))))
    (buffer-string)))

(defun posthtml-find (content selector)
  "Query CONTENT for SELECTOR.

Wrapper function to `enlive-query'."
  (enlive-query content selector))

(defun posthtml-find-all (content selector)
  "Query CONTENT for every SELECTOR.

Wrapper function to `enlive-query-all'"
  (enlive-query-all content selector))

(defun posthtml-append (container &optional element)
  "To CONTAINER, append ELEMENT.

CONTAINER is an esxml list, ELEMENT is a list or a string."
  (when element
    (if (> 3 (length container))
        (setcdr container (list nil element))
      (setf (nthcdr 2 container)
            (list (nth 2 container) element)))))

(defun posthtml-prepend (container &optional element)
  "To CONTAINER, prepend ELEMENT.

CONTAINER is an esxml list, ELEMENT is a list or a string."
  (when element
    (if (> 3 (length container))
        (posthtml-append container element)
      (setf (nthcdr 2 container)
            (append (list element) (nthcdr 2 container))))
    container))

(defun posthtml-attribute (element attribute &optional value force)
  "Return ELEMENT's ATTRIBUTE value.

With VALUE argument, add to ATTRIBUTE, VALUE.  Optional FORCE sets
ATTRIBUTE to VALUE.  ELEMENT is an `esxml' list."
  (let ((attribute (or (and (string= ":" (substring (format "%s" attribute) 0 1))
                            (intern (substring (format "%s" attribute) 1 nil)))
                       (identity attribute))))
    (if value
        (let* ((current-attribute (assq attribute (nth 1 element)))
               (current-value (when current-attribute (cdr current-attribute))))
          (if (and current-value (not force))
              (posthtml-attribute-set element attribute (format "%s %s" current-value value))
            (posthtml-attribute-set element attribute (format "%s" value))))
      (let ((attributes (nth 1 element)))
        (when (and (not (null attributes))
                   (listp attributes))
          (cdr (assq attribute attributes)))))))

(defun posthtml-attributes (element &rest attributes)
  "Apply ELEMENT to `posthtml-attribute' with each of ATTRIBUTES."
  (seq-reduce (lambda (element attribute)
                (apply 'posthtml-attribute element attribute))
              (seq-partition attributes 2)
              element))

(defun posthtml-attributes-set (element &rest attributes)
  "Apply ELEMENT to `posthtml-attribute-set' with each of ATTRIBUTES."
  (seq-reduce (lambda (element attribute)
                (apply 'posthtml-attribute element (append attribute '(t))))
              (seq-partition attributes 2)
              element))

(defun posthtml-attribute-set (element attribute value)
  "Set ELEMENT's ATTRIBUTE to VALUE."
  (let ((attributes (nth 1 element)))
    (if (or (null value) (string= "" value))
        (when (and (not (null attributes))
                   (listp attributes))
          (let ((current-attribute (assq attribute attributes)))
            (when current-attribute
              (setf (nth 1 element)
                    (remove current-attribute attributes)))))
      (let ((value (format "%s" value)))
        (cond ((null attributes)
               (setf (nth 1 element)
                     (list (cons attribute value))))
              ((listp attributes)
               (setf (nth 1 element) (append (delq (assq attribute attributes) attributes)
                                             (list (cons attribute value)))))))
        element)))

(provide 'posthtml)
;;; posthtml.el ends here

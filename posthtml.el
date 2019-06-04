;;; posthtml.el --- decorate HTML string DOM with CSS-like selectors -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Elo Laszlo

;; Author: Elo Laszlo <hello at bald dot cat>
;; Created: December 2018
;; Package-Version: 0.6.0
;; Keywords: files
;; Homepage: http://bald.cat/posthtml
;; Package-Requires: ((emacs "25.3.1") (esxml "20171129.807"))
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
;;
;; posthtml provides a fluent interface to decorating a
;; string of HTML, like "<ul><li/><li/></ul>" - to add
;; attributes such as class or id to elements, or to do
;; custom processing.
;;
;;     (posthtml-decorate "<ul><li/><li/></ul>"
;;     		               '(:id ul "ID")
;;     		               '(:class each "ul li" "CLASSNAME")
;;     		               '((lambda (li)
;;     		                  (posthtml-append li "CONTENT"))
;;     		                  "ul>li"))
;;
;; The string is parsed into an esxml (DOM) tree structure
;; with libxml-parse-html-region - the defined decorators
;; are applied in sequence, and a string rendered via
;; esxml-to-xml is returned. Querying is done with
;; esxml-query.

;;; Code:
(require 'esxml)
(require 'esxml-query)
(require 'dom)

(defvar posthtml-decorator-token-alist
  '((append . posthtml-append)
    (prepend . posthtml-prepend)
    (set . posthtml-set)
    (add . posthtml-add)
    (id (lambda (node value) (posthtml-set node :id value)))
    (class (lambda (node value) (posthtml-set node :class value)))))

;;;###autoload
(defun posthtml-decorate (content &rest decorators)
  "Translates html based CONTENT into an `esxml' parse-tree,
applies (each) DECORATORS, returns string."
  (let* ((html-string (substring-no-properties content))
         (parse-tree (seq-reduce
                      (lambda (content decorator)
                        (let* ((fn (if (eq 'each (cadr decorator))
                                       'posthtml-apply-each
                                     'posthtml-apply))
                               (action (format "%s" (car decorator)))
                               (action (if (string-prefix-p ":" action)
                                           (intern-soft (substring action 1 nil))
                                         (car decorator)))
                               (action (or (cdr (assoc action posthtml-decorator-token-alist))
                                           action))
                               (action (if (functionp action) action
                                         (car action)))
                               (args (if (eq 'each (cadr decorator))
                                         (cddr decorator)
                                       (cdr decorator))))
                          (apply fn content action args)))
                      decorators
                      (posthtml--html-to-esxml html-string))))
    (concat
     (save-match-data
       (if (eq nil (string-match "^\\(<!DOCTYPE[^>]+?>\n?\\)" html-string))
           "" (match-string 1 html-string)))
     (posthtml--esxml-to-html
      (if (eq nil (string-match "<html" html-string))
          (if (eq nil (string-match "<body" html-string))
              (caddr (posthtml-find parse-tree 'body))
            (posthtml-find parse-tree 'body))
        parse-tree)))))

(defun posthtml-apply (node fn selector &rest args)
  "Search NODE, apply FN with element found by SELECTOR and ARGS.

NODE is an `esxml' parse-tree."
  (let ((element (posthtml-find node selector)))
    (setf element (apply fn element args))
    node))

(defun posthtml-apply-each (node fn selector &rest args)
  "Search NODE, apply FN to each element found by SELECTOR and ARGS.

NODE is an `esxml' parse-tree."
  (dolist (element (posthtml-find-all node selector)
                   node)
    (setf element (apply fn element args))))

(defun posthtml-find (content selector)
  "Search CONTENT for css SELECTOR, with `esxml-query'."
  (esxml-query (or (and (not (listp selector))
                        (esxml-parse-css-selector (format "%s" selector)))
                   selector)
               content))

(defun posthtml-find-all (content selector)
  "Search CONTENT for css SELECTOR, with `esxml-query-all'."
  (esxml-query-all (or (and (not (listp selector))
                            (esxml-parse-css-selector (format "%s" selector)))
                       selector)
                   content))

(defun posthtml-append (container &optional element)
  "Append to CONTAINER, as the last child node, ELEMENT.

CONTAINER is and `esxml' parse-tree.  ELEMENT can be a
string or an esxml element."
  (dom-append-child container element))

(defun posthtml-prepend (container &optional element)
  "Prepend to CONTAINER, as the first child node, ELEMENT.

CONTAINER is and `esxml' parse-tree.  ELEMENT can be a
string or an esxml element."
  (dom-add-child-before container element nil))

(defun posthtml-attr (element attribute &optional value)
  "Return ELEMENT ATTRIBUTE value.  With optional VALUE, set.

ELEMENT is an `esxml' parse-tree.  ATTRIBUTE is a string,
optionally prepended with an ':'.  Setting VALUE to an empty
string, '', removes ATTRIBUTE."
  (let ((attr-name (format "%s" attribute)))
    (when (string-prefix-p ":" attr-name)
      (setq attribute (intern (substring attr-name 1 nil)))))
  (or (when (and value (not (string= "" value)))
        (dom-set-attribute element attribute value)
        element)
      (when (string= "" value)
        (let* ((attributes (dom-attributes element))
               (attribute (assq attribute attributes)))
          (dom-set-attributes element (remove attribute attributes)))
        element)
      (dom-attr element attribute)))

(defun posthtml-set (element &rest values)
  "Set ELEMENT attributes to VALUES.

ELEMENT is an `esxml' parse-tree.  VALUES is a list of
attributes and values."
  (seq-reduce (lambda (element attribute)
                (apply 'posthtml-attr element attribute))
              (seq-partition values 2) element))

(defun posthtml-add (element &rest values)
  "Add to ELEMENT attributes, VALUES.

ELEMENT is an `esxml' parse-tree.  VALUES is a list of
attributes and values."
  (seq-reduce (lambda (element attribute)
                (let ((current-value (posthtml-attr element (car attribute))))
                  (posthtml-attr element (car attribute)
                                 (if current-value
                                     (format "%s %s" current-value (cadr attribute))
                                   (format "%s" (cadr attribute))))))
              (seq-partition values 2) element))

(defun posthtml--html-to-esxml (contents)
  "Translate CONTENTS, a string of html, into an `esxml' parse-tree."
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
  "Translate CONTENTS, an `esxml' parse-tree, to string."
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

(provide 'posthtml)
;;; posthtml.el ends here

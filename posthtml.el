;;; posthtml.el --- post HTML rendering filters for org-export
;; Copyright (C) Elo Laszlo 2017

;; Author: Elo Laszlo <laszlo at manifold dot io>
;; Created: August 2016
;; Updated: Feb 2017
;; Description: post HTML rendering filters for org-export
;; Homepage: http://manifold.io/project/posthtml
;; Version: 0.2.0
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
;;
;; posthtml defines post rendering hooks for org-export;
;; it works with a parse-tree representation of the current page,
;; providing programmatic access to it's DOM.
;;
;; (posthtml (posthtml/doctype "<!DOCTYPE html>")
;;           (posthtml/head-title (posthtml: title))
;;           (posthtml$ [html body article:first] "add text")
;;           [...])

;;; Code:
(require 'esxml)
(require 'enlive)

(defvar posthtml-doctype '())
(defvar posthtml-special-chars '("amp" "gt" "lt"))


(defmacro posthtml-add-export-filter-final-output (&rest body)
  `(add-to-list 'org-export-filter-final-output-functions
                (posthtml* (posthtml ,@body))))

(defmacro posthtml-export-filter-final-output (&rest body)
  `(lambda () (posthtml-add-export-filter-final-output ,@body)))

(defmacro posthtml (&rest body)
  `(lambda (contents &optional info)
     (when (not (listp contents))
       (let ((doctype (subseq contents 0 (search "<html" contents))))
         (when (not (string= "" doctype)) (posthtml/doctype doctype)))
       (setf contents (posthtml-parse-html contents)))
     ,@body
     (if (listp contents) (setf contents (posthtml-parse-esxml contents))
       (when posthtml-doctype
         (setf contents (concatenate 'string posthtml-doctype "\n" contents))))
     contents))

(defun posthtml* (fn)
  `(lambda (contents backend info) (funcall ,fn contents info)))


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
    (when posthtml-doctype (insert posthtml-doctype "\n"))
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
  (let ((name (mapconcat (lambda (s)
                           (let ((s (format "%s" s))) (subseq s 0 (search ":" s))))
                         (cdr (append selector nil)) "-")))
    `(defun ,(intern (concat "posthtml/" name)) (&optional content)
       (posthtml$ ,selector content))))

(defun posthtml/doctype (&optional doctype)
  (if doctype (setf posthtml-doctype (subseq doctype 0 (search "\n" doctype :from-end t)))
    (setf posthtml-doctype "<!DOCTYPE html>")))

(def-posthtml/element [html head:first title])


(defmacro posthtml: (key)
  (let ((key (intern (concat ":" (format "%s" key)))))
    `(let ((value (plist-get info ,key)))
       ,(if (member key '(:title :author))
            '(if (and value (listp value)) (substring-no-properties (car value))
               (or value ""))
          '(or value "")))))


(provide 'posthtml)

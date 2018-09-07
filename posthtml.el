;;; posthtml.el --- post HTML rendering filters for org-export
;; Copyright (C) Elo Laszlo 2017

;; Author: Elo Laszlo <hello at bald dot cat>
;; Created: August 2016
;; Updated: April 2018
;; Description: post HTML rendering filters for org-export
;; Homepage: http://bald.cat/posthtml
;; Version: 0.4.0
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
;; An emacs org-mode export filter for cosmetic touchups of html output;
;; a set of macros that give access to the DOM structure, allowing for
;; querying and modification with CSS like selectors.
;;
;; ($each [header nav li]
;;        (lambda (nav-element uri)
;;          (let ((href (posthtml$ nav-element [a] :attr 'href)))
;;            (when (and (not (string= "/" href))
;;                       (string-prefix-p href uri t))
;;              (posthtml-attribute nav-element 'class 'current))))
;;        (page-uri info))

;;; Code:
(require 'esxml)                        ; esxml-to-xml
(require 'enlive)                       ; enlive-query, enlive-query-all
(eval-when-compile (require 'cl))       ; lexical-let, cl-macrolet

;; macros
;; #+BEGIN_EXAMPLE
;;   (posthtml:filter-final-output [...])
;; #+END_EXAMPLE

;;;###autoload
(defmacro posthtml:filter-final-output (&rest body)
  "add a posthtml filter to the default export process

see org-export-filter-final-output-functions and
    posthtml:filter"
  `(add-to-list 'org-export-filter-final-output-functions
                (lambda (contents backend info)
                  (posthtml:filter contents info ,@body))))

;;;###autoload
(defmacro posthtml:filter (content &optional export-options &rest body)
  "parse CONTENT with BODY

EXPORT-OPTIONS is lexically bound as INFO,
CONTENT is lexically bounda as CONTENTS"
  (let ((contents (make-symbol "contents"))
        (info (make-symbol "info")))
    `(funcall (lambda (,contents &optional ,info)
                (lexical-let ((doctype (save-match-data
                                         (if (eq nil (string-match "^\\(<!DOCTYPE[^>]+?>\n?\\)" ,contents)) ""
                                           (match-string 1 ,contents))))
                              (contents (posthtml--html-to-esxml ,contents))
                              (info ,info))
                  (cl-macrolet (($each (selector &optional fn &rest args)
                                       `(funcall 'posthtml$each contents ,selector ,fn ,@args))
                                ($ (selector &optional fn &rest args)
                                   `(funcall 'posthtml$ contents ,selector ,fn ,@args)))
                    ,@body)
                  (concat doctype (posthtml--esxml-to-html contents))))
              ,content ,export-options)))

;; selectors
;; *posthtml$ (container selector &optional fn &rest args)*
;; #+BEGIN_EXAMPLE
;;   (posthtml$ esxml-container [html] 'posthtml-append "this")
;;   ($ [html] :append "this")
;;   ($ [html] (lambda (container this) (posthtml-append container this)) "this")
;; #+END_EXAMPLE

(defun posthtml$ (container selector &optional fn &rest args)
  "Query CONTAINER for SELECTOR; apply found element and ARGS
to FN, return results.

CONTAINER is an esxml list, SELECTOR is a vector; ie [html body]."
  (apply (posthtml--fn-tokens fn)
         (enlive-query container selector)
         args))


;; *posthtml$each (container selector &optional fn &rest args)*
;; #+BEGIN_EXAMPLE
;;   (posthtml$each esxml-container [body p] 'posthtml-append "this")
;;   ($each [body p] :append "this")
;;   ($each [body p] (lambda (element this) (posthtml-append element this)) "this")
;; #+END_EXAMPLE

(defun posthtml$each (container selector &optional fn &rest args)
  "Query CONTAINER for SELECTOR; apply each found element and
ARGS to FN, return results.

CONTAINER is an esxml list, SELECTOR is a vector; ie [html body]."
  (let ((fn (posthtml--fn-tokens fn)))
    (mapcar (lambda (element) (apply fn element args))
            (enlive-query-all container selector))))

;; dom manipulation
;; :PROPERTIES:
;; :header-args+: :comments org
;; :END:
;; - posthtml-append (container &optional element)

(defun posthtml-append (container &optional element)
  "Append ELEMENT to CONTAINER.

CONTAINER is an esxml list, ELEMENT is a list or a string."
  (when element
    (if (= 1 (length container))
        (nconc container (list '() element))
      (nconc container (list element)))))


;; - posthtml-prepend (container &optional element)

(defun posthtml-prepend (container &optional element)
  "Prepend ELEMENT to CONTAINER.

CONTAINER is an esxml list, ELEMENT is a list or a string."
  (when element
    (if (> 3 (length container))
        (posthtml-append container element)
      (setf (nthcdr 2 container)
            (append (list element) (nthcdr 2 container))))
    container))

;; attributes
;; - posthtml-attribute (element attribute &optional values)

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


;; - posthtml-attribute-set (element attribute &optional values)

(defun posthtml-attribute-set (element attribute &optional values)
  (let ((attributes (nth 1 element))
        (new-attribute (make-symbol (format "%s" attribute)))
        (value (format "%s" (or values ""))))
    (if (not (string= "" value))
        (cond ((null attributes)
               (setf (nth 1 element)
                     (list (cons new-attribute value))))
              ((listp attributes)
               (let ((current-attribute (assoc attribute (nth 1 element))))
                 (if current-attribute
                     (setcdr current-attribute value)
                   (setf (nth 1 element)
                         (nconc (nth 1 element)
                                (list (cons new-attribute value))))))))
      (when (and (not (null attributes))
                 (listp attributes))
        (let ((current-attribute (assoc attribute attributes)))
          (when current-attribute
            (setf (nth 1 element)
                  (remove current-attribute attributes))))))))

;; private functions                             :noexport:

(defun posthtml--html-to-esxml (contents)
  (with-temp-buffer
    (insert (substring-no-properties contents))
    (loop for token in
          '(("&amp;" . "%%amp%%")
            ("&lt;" . "%%lt%%")
            ("&gt;" . "%%gt%%"))
          do
          (goto-char (point-min))
          (while (search-forward (car token) nil t)
            (replace-match (cdr token) nil t)))
    (libxml-parse-html-region (point-min) (point-max))))

(defun posthtml--esxml-to-html (contents)
  (with-temp-buffer
    (insert (esxml-to-xml contents))
    (loop for token in
          '(("&amp;" . "%%amp%%")
            ("&lt;" . "%%lt%%")
            ("&gt;" . "%%gt%%"))
          do
          (goto-char (point-min))
          (while (search-forward (cdr token) nil t)
            (replace-match (car token) nil t)))
    (save-excursion
      (while (search-forward "<comment>" nil t) (replace-match "<!--" nil t)))
    (save-excursion
      (while (search-forward "</comment>" nil t) (replace-match "-->" nil t)))
    (loop for element in
          '("figure" "a" "script" "style")
          do
          (goto-char (point-min))
          (while (search-forward-regexp (format "<\\(%s\\).*?\\(/>\\)" element) nil t)
            (replace-match "></\\1>" nil nil nil 2)))
    (buffer-string)))

(defun posthtml--fn-tokens (&optional fn)
  (if (not fn) 'identity
    (or (plist-get
         '(:append posthtml-append :prepend posthtml-prepend :attr posthtml-attribute) fn)
        fn)))

(provide 'posthtml)
;;; posthtml.el ends here

;;; org-html-post.el --- post HTML rendering filters for org-export
;; Copyright (C) Elo Laszlo 2016

;; Author: Elo Laszlo <laszlo at manifold dot io>
;; Created: August 2016
;; Description: post HTML rendering filters for org-export
;; Homepage: http://manifold.io/project/org-html-post
;; Version: 0.1.0
;; Package-Requires: ((esxml "20160703.1417")(enlive "20150824.549")(s "20160711.525"))
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
;; org-html-post defines post rendering hooks for org-export;
;; it works with a parse-tree representation of the current page,
;; providing programmatic access to it's DOM.
;;
;; (org-html-post-add-filter
;;  '(when (s-starts-with "/blog" (org-html-post--page-uri info))
;;     (org-html-post-add-attribute (enlive-query contents [body])
;;                                  'class 'dark-theme)))
;;

;;; Code:
(require 'ox-publish)
(require 'esxml)
(require 'enlive)
(require 'cl)
(require 's)

;;
;; setup
;;

;;;###autoload
(defun org-html-post (contents body &optional info)
  "Evaluates BODY within the context of CONTENTS and INFO.

During evaluation, CONTENTS are translated
into a parse-tree representation through
libxml-parse-html-region.

The final output is returned as a string,
parsed by esxml-to-xml."
  (if (not (null body))
      (let ((contents (enlive-parse
                       (s-replace-all
                        '(("&amp;" . "%%AMP%%")("&gt;" . "%%GT%%")("&lt;" . "%%LT%%"))
                        contents)))
            (body (append body '((if (listp contents)
                                     (esxml-to-xml contents)
                                   contents)))))
        (s-replace-all
         '(("%%AMP%%" . "&amp;")("%%GT%%" . "&gt;")("%%LT%%" . "&lt;"))
         (eval body)))
    contents))


;;;###autoload
(defun org-html-post-filter (&rest filters)
  "Sets up an org-html-post FILTER chain.

Returns a function which can be utilized as
an org-export-filter-final-output-function."
  `(lambda (contents backend info)
     (org-html-post contents ',(cons 'progn filters) info)))


;;;###autoload
(defun org-html-post-add-filter (&rest filters)
  "Adds FILTERS as a chain to the end of
an org export or publishing process.

FILTERS are run for each document; each frame
is provided direct access to CONTENTS and INFO."
  (add-to-list 'org-export-filter-final-output-functions
               (apply 'org-html-post-filter filters)))



;;
;; filters
;;

(defun org-html-post-add-attribute (element attribute value)
  "Adds ATTRIBUTE with VALUE to ELEMENT unless it is already defined.

ELEMENT is a parse-tree, as returned by libxml-parse-html-region.
ATTRIBUTE and VALUE are strings."
  (let ((new-attribute (make-symbol (format "%s" attribute)))
        (value (format "%s" value)))
    (cond ((null (nth 1 element))
           (setf (nth 1 element)
                 (list (cons new-attribute value))))
          ((listp (nth 1 element))
           (let ((current-attributes (assoc attribute (nth 1 element))))
             (if (null current-attributes)
                 (setf (nth 1 element)
                       (nconc (nth 1 element) (list (cons new-attribute value))))
               (when (not (member value (split-string (cdr current-attributes))))
                 (setcdr current-attributes
                         (format "%s %s" (cdr current-attributes) value)))))))
    element))



(defun org-html-post-add-doctype (&optional doctype)
  "Appends DOCTYPE to current value of CONTENTS.

CONTENTS is converted from parse-tree to an HTML string, when required.
Default for DOCTYPE is <!DOCTYPE html>."
  (when (listp contents)
    (setq contents (esxml-to-xml contents)))
  (setq contents (concat (or doctype "<!DOCTYPE html>")
                         "\n"
                         contents)))



(defun org-html-post-add-head-author (&optional author)
  "Adds an author meta tag before the HEAD > TITLE tag.

AUTHOR is an optional string - the default value is
defined by the org-publish project; it can be set
using the :author property."
  (let* ((author (or author (let ((info-author (plist-get info :author)))
                              (when (listp info-author)
                                (substring-no-properties (car info-author))))))
         (tag `(meta ((name . "author") (content . ,author)))))
    (org-html-post-insert-before [title]
                                 (enlive-query contents [head])
                                 tag)))



(defun org-html-post-add-head-title (&optional template title)
  "Prepends TITLE to contents of HEAD > TITLE, using TEMPLATE

TEMPLATE is an optional string that is supplied to format.
TITLE is an optional string - the default value is
retreived from the document level #+TITLE: property."
  (let ((title (or title (let ((info-title (plist-get info :title)))
                           (when (listp info-title)
                             (substring-no-properties (car info-title))))))
        (template (or template "%s - ")))
    (when title
      (org-html-post-insert-at 0
                               (enlive-query contents [head title])
                               (format template title)))))



(defun org-html-post-append-javascript (&rest paths)
  "Appends an HTML script tag with the contents
of all PATHS, to the body tag.

PATHS are strings; they are relative to :base-directory."
  (org-html-post-insert
   (enlive-query contents [body])
   `(script ((type . "application/javascript")
                 (language . "javascript"))
                ,(with-temp-buffer
                   (dolist (path paths)
                     (insert-file-contents
                      (expand-file-name path (plist-get info :base-directory))))
                   (buffer-string)))))



(defun org-html-post-insert (container &rest new-children)
  "Appends NEW-CHILDREN onto CONTAINER's contents.

CONTAINER and NEW-CHILDREN are parse-trees, as
returned by libxml-parse-html-region; NEW-CHILDREN
elements can also be strings."
  (setf container (nconc container new-children)))



(defun org-html-post-insert-at (insertion-point container &rest new-children)
  "Inserts NEW-CHILDREN into CONTAINER at INSERTION-POINT.

CONTAINER and NEW-CHILDREN are parse-trees, as
returned by libxml-parse-html-region; NEW-CHILDREN
elements can also be strings.
INSERTION-POINT is a digit; negative numbers prepend
NEW-CHILDREN to CONTAINER's contents."
  (if (<= insertion-point 0)
      (setf (cddr container) (append new-children (cddr container)))
    (let ((insertion-point (- (length (cddr container)) insertion-point)))
      (setf (cddr container) (append (butlast (cddr container) insertion-point)
                                     new-children
                                     (last (cddr container) insertion-point))))))



(defun org-html-post-insert-after (sibling-selector container &rest new-children)
  "Inserts NEW-CHILDREN into CONTAINER after the
element found by SIBLING-SELECTOR.

CONTAINER and NEW-CHILDREN are parse-trees, as
returned by libxml-parse-html-region; NEW-CHILDREN
elements can also be strings.
SIBLING-SELECTOR is supplied to enlive-query, an
example query is [BODY .class]."
  (let* ((sibling (enlive-with container (enlive-query sibling-selector)))
         (insertion-point (+ 1 (cl-position sibling (nthcdr 2 container)))))
    (apply 'org-html-post-insert-at insertion-point container new-children)))



(defun org-html-post-insert-before (sibling-selector container &rest new-children)
  "Inserts NEW-CHILDREN into CONTAINER before the
element found by SIBLING-SELECTOR.

CONTAINER and NEW-CHILDREN are parse-trees, as
returned by libxml-parse-html-region; NEW-CHILDREN
elements can also be strings.
SIBLING-SELECTOR is supplied to enlive-query, an
example query is [BODY .class]."
  (let* ((sibling (enlive-with container (enlive-query sibling-selector)))
         (insertion-point (cl-position sibling (nthcdr 2 container))))
    (apply 'org-html-post-insert-at insertion-point container new-children)))



(defun org-html-post-mark-current-links (&optional selector mark)
  "Decorates HTML link elements with a CSS
class attribute containing MARK, when their
URI is within the current path.

Useful for highlighting the current path in
navigation elements, such as when SELECTOR is
set as [nav a]. MARK is a string. SELECTOR is
supplied to enlive-query."
  (let ((selector (or selector [a]))
        (mark (or mark "current")))
    (mapc (lambda (link)
            (let ((uri (or (enlive-attr link 'href) "")))
              (when (org-html-post--in-current-path-p uri info)
                (org-html-post-add-attribute link 'class mark))))
          (enlive-query-all contents selector))))



(defun org-html-post-remove-whitespace ()
  "Removes whitespace between HTML elements,
using the current value of CONTENTS.

CONTENTS is converted from parse-tree to
an HTML string, when required."
  (when (listp contents)
    (setq contents (esxml-to-xml contents)))
  (setq contents (s-replace-all '((">\n" . ">") ("\n<" . "<")) contents)))



(defun org-html-post-set-contents (element &rest contents)
  "Replaces an ELEMENT's contents with
the new children as defined in CONTENTS.

ELEMENT and CONTENTS are parse-trees, as
returned by libxml-parse-html-region; CONTENTS
can also be strings, an empty string, or nil."
  (if (or (null (car contents))
          (and (stringp (car contents)) (string= "" (car contents))))
      (setcdr element (cadr element))
    (setf (nthcdr 2 element) contents)))



;;
;; utility
;;

(defun org-html-post--page-uri (info)
  "Retrieves the current page's URI, relative to root.

INFO is a plist holding contextual information."
  (format "/%s" (s-chop-prefix
                 (expand-file-name (plist-get info :publishing-directory))
                 (plist-get info :output-file))))



(defun org-html-post--in-current-path-p (uri info)
  "Determines whether or not URI is in the current page's path.

URI is a string. INFO is a plist holding contextual information."
  (let* ((current-uri (org-html-post--page-uri info))
         (shared-start (s-shared-start uri current-uri)))
    (cond ((string= uri current-uri) t)
          ((and (s-ends-with-p "index.html" current-uri)
                (string= (format "%sindex.html" uri) current-uri)) t)
          ((and (s-ends-with-p "index.html" uri)
                (string= (format "%sindex.html" current-uri) uri)) t)
          ((not (s-starts-with-p "/" uri)) t)
          ((not (or (string= "" shared-start) (string= "/" shared-start))) t))))



(defun org-html-post--project-index-p (info)
  "Is this the project's /index.html page?

INFO is a plist holding contextual information."
  (string= "/index.html" (org-html-post--page-uri info)))



(provide 'org-html-post)
;;; org-html-post.el ends here

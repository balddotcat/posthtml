
(require 'package)
(package-initialize)
(require 'ox-publish)

(load-file "posthtml.el")

(ert-deftest posthtml-add-filter-to-export-process ()
  "org export (derived) backend"
  (org-export-define-derived-backend
      'test1 'html
    :filters-alist `((:filter-final-output
                      ,(posthtml (setq contents "THIS")))))
  (should
   (string= "THIS"
            (with-temp-buffer (org-export-as 'test1 nil nil nil))))

  "default export process"
  (setf org-export-filter-final-output-functions nil)
  (posthtml-filter-export-output (setq contents "THIS"))
  (should
   (string= "THIS"
            (with-temp-buffer (org-export-as 'html nil nil nil))))

  "org publishing process :preparation-action"
  (setf org-export-filter-final-output-functions nil)
  (funcall (posthtml-add-export-filter (setq contents "THIS")))
  (should
   (string= "THIS"
            (with-temp-buffer (org-export-as 'html nil nil nil))))

  (setf org-export-filter-final-output-functions nil))


(ert-deftest posthtml/doctype ()
  "does not insert doctype if not already declared"
  (let ((filter (posthtml (and t))))
    (should
     (string-prefix-p "<html"
                      (funcall filter "<html/>"))))

  "uses INFO to get current doctype"
  (let ((filter (posthtml (and t))))
    (should
     (string-prefix-p "<!DOCTYPE html>"
                      (funcall filter
                               "<!DOCTYPE this><html/>"
                               nil
                               '(:html-doctype "html5"))))))


(ert-deftest posthtml-special-characters ()
  (let ((filter (posthtml (and t))))
    (should
     (string= "<html><body>&amp; &gt; &lt;</body></html>"
              (funcall filter "<html><body>&amp; &gt; &lt;</body></html>")))
    (should
     (string= "<html><body><!-- comment --></body></html>"
              (funcall filter "<html><body><!-- comment --></body></html>")))))


(ert-deftest posthtml-append ()
  "function, accepting ELEMENT and ARGS"
  (let ((filter (posthtml
                 (posthtml$ contents [html] 'posthtml-append "this")
                 ($ [html] :append "that"))))
    (should
     (string= "<html>thisthat</html>"
              (funcall filter "<html/>")))))


(ert-deftest posthtml-prepend ()
  "function, accepting ELEMENT and ARGS"
  (let ((filter (posthtml
                 (posthtml$ contents [html] 'posthtml-prepend "this")
                 ($ [html] :prepend "that"))))
    (should
     (string= "<html>thatthis</html>"
              (funcall filter "<html/>")))))


(ert-deftest posthtml-attribute ()
  (let ((filter (posthtml
                 ($ [body] :attr 'class 'this)))
        (input "<html><body/></html>")
        (expected-result "<html><body class=\"this\"/></html>"))
    (should
     (string= expected-result (funcall filter input))))

  (let ((filter (posthtml
                 ($ [body] :attr 'class 'that)
                 ($ [body] :attr 'id 'this)))
        (input "<html><body class=\"this\"/></html>")
        (expected-result "<html><body class=\"this that\" id=\"this\"/></html>"))
    (should
     (string= expected-result (funcall filter input)))))


(ert-deftest posthtml-current-nav-elements ()
  "example use - with lambda"
  (let ((filter (posthtml
                 ($each [nav li]
                        (lambda (nav-element uri)
                          (when (string-prefix-p uri (posthtml$ nav-element [a] :attr 'href))
                            (posthtml-attribute nav-element 'class 'this)))
                        "/this")))
        (input (concat
                "<html><body>"
                "<nav><ul>"
                "<li><a href=\"/not\">no</a></li>"
                "<li><a href=\"/this\">yes</a></li>"
                "<li><a href=\"/that\">no</a></li>"
                "</ul></nav>"
                "</body></html>"))
        (expected-result (concat
                          "<html><body>"
                          "<nav><ul>"
                          "<li><a href=\"/not\">no</a></li>"
                          "<li class=\"this\"><a href=\"/this\">yes</a></li>"
                          "<li><a href=\"/that\">no</a></li>"
                          "</ul></nav>"
                          "</body></html>")))
    (should
     (string= expected-result (funcall filter input)))))

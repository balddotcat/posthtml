(require 'package)
(package-initialize)
(require 'ox)

(load-file "posthtml.el")

(ert-deftest posthtml-append ()
  (should (string= "<html>thisthat</html>"
                   (posthtml:filter "<html/>" nil
                                    (posthtml$each contents [html] 'posthtml-append "this")
                                    ($ [html] :append "that")))))

(ert-deftest posthtml-prepend ()
  (should (string= "<html>thatthis</html>"
                   (posthtml:filter "<html/>" nil
                                    (posthtml$each contents [html] 'posthtml-prepend "this")
                                    ($ [html] :prepend "that")))))

(ert-deftest posthtml-attribute ()
  (should (string= "<html><body class=\"this that\" id=\"this\"/></html>"
                   (posthtml:filter "<html><body class=\"this\"/></html>" nil
                                    ($ [body] :attr 'class 'that)
                                    ($ [body] :attr 'id 'this)))))

(ert-deftest posthtml-set-attribute ()
  (should (string= "<html><body class=\"that\"/></html>"
                   (posthtml:filter "<html><body class=\"this\"/></html>" nil
                                    ($ [body] 'posthtml-attribute-set 'class 'that))))

  (should (string= "<html><body id=\"id\"/></html>"
                   (posthtml:filter "<html><body id=\"id\" class=\"this\"/></html>" nil
                                    ($ [body] 'posthtml-attribute-set 'class "")))))

(ert-deftest posthtml--doctype ()
  (should (string= "<html/>" (posthtml:filter "<html/>")))
  (should (string= "<!DOCTYPE html><html/>" (posthtml:filter "<!DOCTYPE html><html/>"))))

(ert-deftest posthtml--special-characters ()
  (should (string= "<html><body>&amp; &gt; &lt;</body></html>"
                   (posthtml:filter "<html><body>&amp; &gt; &lt;</body></html>")))
  (should (string= "<html><body><!-- comment --></body></html>"
                   (posthtml:filter "<html><body><!-- comment --></body></html>"))))

(ert-deftest posthtml--current-nav ()
  (should (string= (concat
                    "<html><body><nav><ul>"
                    "<li><a href=\"/one\">one</a></li>"
                    "<li class=\"current\"><a href=\"/two\">two</a></li>"
                    "<li><a href=\"/three\">three</a></li>"
                    "</ul></nav></body></html>")
                   (posthtml:filter (concat
                                     "<html><body><nav><ul>"
                                     "<li><a href=\"/one\">one</a></li>"
                                     "<li><a href=\"/two\">two</a></li>"
                                     "<li><a href=\"/three\">three</a></li>"
                                     "</ul></nav></body></html>")
                                    nil
                                    ($each [nav li]
                                           (lambda (nav-element current-uri)
                                             (when (string= (posthtml$ nav-element [a] :attr 'href)
                                                            current-uri)
                                               (posthtml-attribute nav-element 'class 'current)))
                                           "/two")))))

(ert-deftest posthtml--filter-final-output ()
  (setf org-export-filter-final-output-functions nil)
  (posthtml:filter-final-output ($ [h2] 'posthtml-attribute-set 'id 'THIS))
  (should (string-match "<h2 id=\"THIS\">"
                        (org-export-string-as "* headline" 'html t '(:with-toc nil))))
  (setf org-export-filter-final-output-functions nil))

(ert-deftest posthtml--special-elements-dont-close ()
  (should (string= (concat "<html><body>"
                           "<figure></figure>"
                           "<a name=\"anchor\"></a>"
                           "<script></script>"
                           "<style type=\"text/css\"></style>"
                           "</body></html>")
                   (posthtml:filter (concat "<html><body>"
                                            "<figure/>"
                                            "<a name=\"anchor\"/>"
                                            "<script/>"
                                            "<style type=\"text/css\"/>"
                                            "</body></html>")))))

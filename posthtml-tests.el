(and (require 'package) (package-initialize))
(load-file "posthtml.el")

(defmacro posthtml-test (name &rest body)
  "Define a posthtml test form."
  `(ert-deftest ,(intern (format "posthtml-test-%s" name)) ()
     ,@body))

(defmacro decorate (input output &rest body)
  "Define a posthtml test to verify INPUT produces OUTPUT, when
`posthtml-decorate' is called with INPUT and BODY."
  `(should (string= ,output
                    (posthtml-decorate ,input ,@body))))

(defmacro render (input &rest body)
  "Print results of calling `posthtml-decorate' with INPUT and
BODY to stdout."
  `(print (posthtml-decorate ,input ,@body)))

(posthtml-test doctype
               (decorate "<html/>" "<html/>")
               (decorate "<!DOCTYPE html><html/>" "<!DOCTYPE html><html/>"))

(posthtml-test special-characters
               (decorate "<html><body>&amp; &gt; &lt;</body></html>"
                         "<html><body>&amp; &gt; &lt;</body></html>")
               (decorate "<html><body><!-- comment --></body></html>"
                         "<html><body><!-- comment --></body></html>"))

(posthtml-test special-elements
               (decorate (concat "<html><body>"
                                 "<figure/>"
                                 "<a name=\"anchor\"/>"
                                 "<script/>"
                                 "<style type=\"text/css\"/>"
                                 "</body></html>")
                         (concat "<html><body>"
                                 "<figure></figure>"
                                 "<a name=\"anchor\"></a>"
                                 "<script></script>"
                                 "<style type=\"text/css\"></style>"
                                 "</body></html>")))

(posthtml-test append
               (decorate "<ul><li/></ul>" "<ul><li>APPEND</li></ul>"
                         '(posthtml-apply [li] posthtml-append "APPEND"))
               (decorate "<ul><li/></ul>" "<ul><li/>APPEND</ul>"
                         '(posthtml-apply [ul] posthtml-append "APPEND"))
               (decorate "<ul><li/></ul>" "<ul><li/>APPEND</ul>"
                         '(posthtml-apply [ul] :append "APPEND"))
               (decorate "<ul><li/></ul>" "<ul><li/>APPEND</ul>"
                         '($ [ul] :append "APPEND")))

(posthtml-test prepend
               (decorate "<h1>title</h1>" "<h1>PREPEND title</h1>"
                         '(posthtml-apply [h1] posthtml-prepend "PREPEND "))
               (decorate "<h1>title</h1>" "<h1>PREPEND title</h1>"
                         '(posthtml-apply [h1] :prepend "PREPEND "))
               (decorate "<h1>title</h1>" "<h1>PREPEND title</h1>"
                         '($ [h1] :prepend "PREPEND ")))

(posthtml-test attribute
               (decorate "<html><body class=\"this\"/></html>"
                         "<html><body class=\"this that\" id=\"this\"/></html>"
                         '(posthtml-apply [body] posthtml-attribute 'class 'that)
                         '(posthtml-apply [body] posthtml-attribute 'id 'this))
               (decorate "<html><body class=\"this\"/></html>"
                         "<html><body class=\"this that\" id=\"this\"/></html>"
                         '(posthtml-apply [body] :attr 'class 'that)
                         '(posthtml-apply [body] :attr 'id 'this))
               (decorate "<html><body class=\"this\"/></html>"
                         "<html><body class=\"this that\" id=\"this\"/></html>"
                         '($ [body] :attr 'class 'that)
                         '($ [body] :attr 'id 'this)))

(posthtml-test set-attribute
               (decorate "<html><body class=\"this\"/></html>"
                         "<html><body class=\"that\"/></html>"
                         '(posthtml-apply [body] posthtml-attribute-set 'class 'that))
               (decorate "<html><body class=\"this\"/></html>"
                         "<html><body class=\"that\"/></html>"
                         '(posthtml-apply [body] :attr-set 'class 'that))
               (decorate "<html><body class=\"this\"/></html>"
                         "<html><body class=\"that\"/></html>"
                         '($ [body] :attr-set 'class 'that))
               (decorate "<html><body id=\"id\" class=\"this\"/></html>"
                         "<html><body id=\"id\"/></html>"
                         '(posthtml-apply [body] posthtml-attribute-set 'class ""))
               (decorate "<html><body id=\"id\" class=\"this\"/></html>"
                         "<html><body id=\"id\"/></html>"
                         '(posthtml-apply [body] :attr-set 'class ""))
               (decorate "<html><body id=\"id\" class=\"this\"/></html>"
                         "<html><body id=\"id\"/></html>"
                         '($ [body] :attr-set 'class "")))

(posthtml-test multiple-attributes
               (decorate "<ul><li/></ul>"
                         "<ul class=\"ONE TWO\"><li>APPEND</li></ul>"
                         '($ [li] :append "APPEND")
                         '($ [ul] :attr 'class 'ONE)
                         '($ [ul] :attr 'class 'TWO))
               (decorate "<ul><li/></ul>"
                         "<ul class=\"ONE TWO\"><li class=\"ONE TWO\" id=\"TWO\"/></ul>"
                         '($ [ul] posthtml-attributes 'class 'ONE 'class 'TWO)
                         '($ [li] :attrs 'class 'ONE 'class 'TWO)
                         '($ [li] :attrs-set 'id 'ONE 'id 'TWO)))

(posthtml-test attribute-tokens
               (decorate "<ul><li/></ul>"
                         "<ul><li id=\"ONE\" class=\"TWO\"/></ul>"
                         '($ [li] :attrs :id 'ONE :class 'TWO)))

(posthtml-test apply-each
               (decorate "<ul><li/><li/><li/></ul>"
                         "<ul><li>APPEND</li><li>APPEND</li><li>APPEND</li></ul>"
                         '($each [ul li] :append "APPEND")))

(posthtml-test readme-example
               (decorate (concat "<html><body>"
                                 "<h1 class=\"ONE\">headline</h1>"
                                 "<ul id=\"ONE\">"
                                 "<li><a href=\"/uri-one\">uri</a></li>"
                                 "<li><a href=\"/uri-two\">uri</a></li>"
                                 "<li><a href=\"/uri-three\">uri</a></li>"
                                 "</ul>"
                                 "</body></html>")
                         (concat "<html><body>"
                                 "<h1 id=\"TWO\">headline APPEND</h1>"
                                 "<ul class=\"COLUMNS COLUMNSWIDTH3\">"
                                 "<li><a href=\"/uri-one\">uri</a></li>"
                                 "<li class=\"CURRENT\"><a href=\"/uri-two\">uri</a></li>"
                                 "<li><a href=\"/uri-three\">uri</a></li>"
                                 "</ul>"
                                 "</body></html>")
                         '($ [h1] :append " APPEND")
                         '($ [ul] :attr :class 'COLUMNS)
                         '($ [h1] :attrs-set :id 'TWO :class "")
                         '($ [ul] :attr-set :id "")
                         '($each [ul li]
                                 (lambda (list-element current-uri)
                                   (let ((link-uri (posthtml-attribute (posthtml-find list-element [a]) 'href)))
                                     (when (string= link-uri current-uri)
                                       (posthtml-attribute list-element :class 'CURRENT))))
                                 "/uri-two")
                         '($ [ul]
                             (lambda (ul)
                               (posthtml-attribute ul :class (format "COLUMNSWIDTH%s"
                                                                     (length (posthtml-find-all ul [li]))))))))

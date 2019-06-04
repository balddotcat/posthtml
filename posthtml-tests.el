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

(posthtml-test doctype
               (decorate "<html/>" "<html/>")
               (decorate "<!DOCTYPE html><html/>" "<!DOCTYPE html><html/>"))

(posthtml-test append
               (decorate "<ul><li/></ul>" "<ul><li>APPEND</li></ul>"
                         '(:append li "APPEND"))
               (decorate "<ul><li/></ul>" "<ul><li/> APPEND</ul>"
                         '(posthtml-append ul " APPEND")))

(posthtml-test prepend
               (decorate "<h1>title</h1>" "<h1>PREPEND title</h1>"
                         '(:prepend h1 "PREPEND "))
               (decorate "<ul><li/></ul>" "<ul>PREPEND <li/></ul>"
                         '(posthtml-prepend ul "PREPEND ")))

(posthtml-test set-attribute
               (decorate "<html><body class=\"this\"/></html>"
                         "<html><body class=\"that\" id=\"id\"/></html>"
                         '(:set body :class "that")
                         '(:set body :id "id"))
               (decorate "<html><body id=\"id\" class=\"this\"/></html>"
                         "<html><body id=\"id\"/></html>"
                         '(posthtml-set body :class ""))
               (decorate "<html><body/></html>"
                         "<html><body id=\"id\" class=\"class2\"/></html>"
                         '(:set body :id "id" :class "class1" :class "class2")))

(posthtml-test add-attribute
               (decorate "<html><body class=\"this\"/></html>"
                         "<html><body class=\"this that\" id=\"this\"/></html>"
                         '(:add body :class "that" :id "this")))

(posthtml-test tokens
               (decorate "<ul/>" "<ul id=\"id\"/>"
                         '(:id ul "id"))
               (decorate "<ul/>" "<ul class=\"class\"/>"
                         '(:class ul "class")))

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
                         '(:append h1 " APPEND")
                         '(:set ul :class "COLUMNS")
                         '(:set h1 :id "TWO" :class "")
                         '(:set ul :id "")
                         '((lambda (list-element current-uri)
                                   (let ((link-uri (posthtml-attr (posthtml-find list-element 'a) 'href)))
                                     (when (string= link-uri current-uri)
                                       (posthtml-set list-element :class "CURRENT"))))
                           each "ul li" "/uri-two")
                         '((lambda (ul)
                             (posthtml-add ul :class (format
                                                      "COLUMNSWIDTH%s"
                                                      (length (posthtml-find-all ul "li")))))
                           "ul")))

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

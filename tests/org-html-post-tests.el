
(require 'package)
(package-initialize)
(load (expand-file-name "../org-html-post.el" (file-name-directory load-file-name)))



(ert-deftest org-html-post ()
  "without any filters returns html string unaltered"
  (should (string=
           "<html><body>this</body></html>"
           (org-html-post "<html><body>this</body></html>" nil)))

  "evals body frames"
  (should (string=
           "<html><body>this</body></html>"
           (org-html-post
            "<html><body></body></html>"
            '(progn (org-html-post-insert
                     (enlive-query contents [body]) "this")
                    (when nil
                      (org-html-post-insert
                       (enlive-query contents [body]) "not this"))))))

  "libxml-parse-html-region helpfully converts the special chars '&amp;&gt;&lt;' to '&><'"
  (should (string= (concat "<html><body>"
                           "<p>this &amp; that</p>"
                           "<p>this &lt; that</p>"
                           "<p>this &gt; that</p>"
                           "<code class=\"lisp\"><pre>"
                           "&amp;&gt;&lt;"
                           "</pre></code></body></html>")
                   (org-html-post (concat "<html><body>"
                                          "<p>this &amp; that</p>"
                                          "<p>this &lt; that</p>"
                                          "<p>this &gt; that</p>"
                                          "<code class=\"lisp\"><pre>"
                                          "&amp;&gt;&lt;"
                                          "</pre></code></body></html>") '(progn)))))




(ert-deftest org-html-post-filter ()
  "returns a lambda function which can be utilized as an org-export-filter-final-output-function"
  (should (eq 'lambda (car (org-html-post-filter nil))))

  "passes filter chain through to org-html-post"
  (should (string=
           "<html><body>this</body></html>"
           (funcall (org-html-post-filter
                     '(org-html-post-insert (enlive-query contents [body]) "this")
                     '(when nil (org-html-post-insert (enlive-query contents [body]) "not this")))
                    "<html><body></body></html>" nil nil))))




(ert-deftest org-html-post-add-filter ()
  :expected-result :failed
  (x-org-html-post-add-filter))




(ert-deftest org-html-post-add-attribute ()
  "sets attribute on element"
  (should (string= "<p class=\"this\"/>"
                   (esxml-to-xml
                    (org-html-post-add-attribute '(p nil) 'class 'this))))

  (should (string= "<p id=\"that\" class=\"this\"/>"
                   (esxml-to-xml
                    (org-html-post-add-attribute '(p ((id . "that"))) 'class 'this))))

  (should (string= "<p class=\"that this\"/>"
                   (esxml-to-xml
                    (org-html-post-add-attribute '(p ((class . "that"))) 'class 'this))))

  (should (string= "<p class=\"this\"/>"
                   (esxml-to-xml
                    (org-html-post-add-attribute '(p ((class . "this"))) 'class 'this)))))




(ert-deftest org-html-post-add-doctype ()
  (should (string=
           "<!DOCTYPE html>\n<html><body/></html>"
           (org-html-post "<html><body/></html>"
                          '(progn (org-html-post-add-doctype))))))




(ert-deftest org-html-post-add-head-author ()
    :expected-result :failed
    (x-org-html-post-add-head-author))




(ert-deftest org-html-post-add-head-title ()
    :expected-result :failed
    (x-org-html-post-add-head-title))




(ert-deftest org-html-post-append-javascript ()
    :expected-result :failed
    (x-org-html-post-append-javascript))




(ert-deftest org-html-post-insert ()
  "appends child element into parent's contents"
  (should (equal
           '(body nil "this")
           (org-html-post-insert '(body nil) "this")))

  (should (equal
           '(body nil "this" "that")
           (org-html-post-insert '(body nil) "this" "that")))

  (should (equal
           '(body nil (p nil "this"))
           (org-html-post-insert '(body nil) '(p nil "this"))))

  (should (equal
           '(body nil (p nil) (p nil "this"))
           (org-html-post-insert '(body nil (p nil)) '(p nil "this"))))

  (should (string=
           "<body><p/><p>this</p></body>"
           (esxml-to-xml (org-html-post-insert '(body nil (p nil)) '(p nil "this"))))))




(ert-deftest org-html-post-insert-at ()
  "inserts child element into parent at insertion-point"
  (should (equal '((p ((class . "this"))) (p nil) (p nil) (p nil))
                 (org-html-post-insert-at
                  -1
                  '(body nil (p nil) (p nil) (p nil))
                  '(p ((class . "this"))))))

  (should (equal '((p ((class . "this"))) (p nil) (p nil) (p nil))
                 (org-html-post-insert-at
                  0
                  '(body nil (p nil) (p nil) (p nil))
                  '(p ((class . "this"))))))

  (should (equal '((p nil) (p ((class . "this"))) (p nil) (p nil))
                 (org-html-post-insert-at
                  1
                  '(body nil (p nil) (p nil) (p nil))
                  '(p ((class . "this"))))))

  (should (equal '((p nil) (p nil) (p ((class . "this"))) (p nil))
                 (org-html-post-insert-at
                  2
                  '(body nil (p nil) (p nil) (p nil))
                  '(p ((class . "this"))))))

  (should (equal '((p nil) (p nil) (p nil) (p ((class . "this"))))
                 (org-html-post-insert-at
                  3
                  '(body nil (p nil) (p nil) (p nil))
                  '(p ((class . "this"))))))

  (should (equal '((p nil) (p nil) (p nil) (p ((class . "this"))))
                 (org-html-post-insert-at
                  4
                  '(body nil (p nil) (p nil) (p nil))
                  '(p ((class . "this")))))))




(ert-deftest org-html-post-insert-after ()
  "inserts child element into parent after sibling"
  (should (equal
           '((p nil) (p ((class . "mark"))) (p ((class . "this"))) (p nil))
           (org-html-post-insert-after [.mark]
                                       '(body nil (p nil) (p ((class . "mark"))) (p nil))
                                       '(p ((class . "this")))))))




(ert-deftest org-html-post-insert-before ()
  "inserts child element into parent before sibling"
  (should (equal
           '((p nil) (p ((class . "this"))) (p ((class . "mark"))) (p nil))
           (org-html-post-insert-before [.mark]
                                        '(body nil (p nil) (p ((class . "mark"))) (p nil))
                                        '(p ((class . "this")))))))




(ert-deftest org-html-post-mark-current-links ()
  (should
   (string=
    (concat "<html><body>"
            "<p>"
            "<a href=\"/a\"/>"
            "<a href=\"/b/this.html\" class=\"current\"/>"
            "<a href=\"/c\"/>"
            "</p>"
            "</body></html>")
    (org-html-post
     (concat "<p>"
             "<a href=\"/a\"/>"
             "<a href=\"/b/this.html\"/>"
             "<a href=\"/c\"/>"
             "</p>")
     '(progn (org-html-post-mark-current-links))
     '(:publishing-directory "/output/" :output-file "/output/b/this.html"))))

  (should
   (string=
    (concat "<html><body>"
            "<nav>"
            "<a href=\"/a\"/>"
            "<a href=\"/b/this.html\" class=\"current\"/>"
            "</nav>"
            "<p>"
            "<a href=\"/a\"/>"
            "<a href=\"/b/this.html\"/>"
            "<a href=\"/c\"/>"
            "</p>"
            "</body></html>")
    (org-html-post
     (concat "<body>"
             "<nav>"
             "<a href=\"/a\"/>"
             "<a href=\"/b/this.html\"/>"
             "</nav>"
             "<p>"
             "<a href=\"/a\"/>"
             "<a href=\"/b/this.html\"/>"
             "<a href=\"/c\"/>"
             "</p>"
             "</body>")
     '(progn (org-html-post-mark-current-links [nav a]))
     '(:publishing-directory "/output/" :output-file "/output/b/this.html"))))

  (should
   (string=
    (concat "<html><body>"
            "<nav>"
            "<a href=\"/a\"/>"
            "<a href=\"/b/this.html\" class=\"this\"/>"
            "</nav>"
            "<p>"
            "<a href=\"/a\"/>"
            "<a href=\"/b/this.html\"/>"
            "<a href=\"/c\"/>"
            "</p>"
            "</body></html>")
    (org-html-post
     (concat "<body>"
             "<nav>"
             "<a href=\"/a\"/>"
             "<a href=\"/b/this.html\"/>"
             "</nav>"
             "<p>"
             "<a href=\"/a\"/>"
             "<a href=\"/b/this.html\"/>"
             "<a href=\"/c\"/>"
             "</p>"
             "</body>")
     '(progn (org-html-post-mark-current-links [nav a] 'this))
     '(:publishing-directory "/output/" :output-file "/output/b/this.html")))))




(ert-deftest org-html-post-remove-whitespace ()
  (should (string=
           "<html><body/></html>"
           (org-html-post "<html>\n<body/>\n</html>"
                          '(progn (org-html-post-remove-whitespace))))))




(ert-deftest org-html-post-set-contents ()
  "replaces element's contents"
  (should (string=
           "<html><body><p class=\"this\"/></body></html>"
           (org-html-post "<html><body><p/><p/></body></html>"
                          '(progn
                             (org-html-post-set-contents
                              (enlive-query contents [body]) '(p ((class . "this"))))))))

  (should (string=
           "<html><body><p class=\"this\"/></body></html>"
           (org-html-post "<html><body/></html>"
                          '(progn
                             (org-html-post-set-contents
                              (enlive-query contents [body]) '(p ((class . "this"))))))))

  "set to nil"
  (should (string=
           "<html><body/></html>"
           (org-html-post "<html><body><p/></body></html>"
                          '(progn
                             (org-html-post-set-contents
                              (enlive-query contents [body]) nil)))))

  "set to empty string"
  (should (string=
           "<html><body/></html>"
           (org-html-post "<html><body><p/></body></html>"
                          '(progn
                             (org-html-post-set-contents
                              (enlive-query contents [body]) ""))))))




(ert-deftest org-html-post--page-uri ()
  (should (string=
           "/this.html"
           (org-html-post--page-uri
            '(:publishing-directory "/output/" :output-file "/output/this.html")))))




(ert-deftest org-html-post--in-current-path-p ()
  "when in current directory"
  (should (org-html-post--in-current-path-p
           "this.html"
           '(:publishing-directory "/output/" :output-file "/output/that.html")))

  "when in root directory"
  (should-not (org-html-post--in-current-path-p
               "/this.html"
               '(:publishing-directory "/output/" :output-file "/output/index.html")))

  "when uri is the same as current-path"
  (should (org-html-post--in-current-path-p
           "/this.html"
           '(:publishing-directory "/output/" :output-file "/output/this.html")))

  "uri is in a subdirectory of current-path"
  (should (org-html-post--in-current-path-p
           "/a/b/this.html"
           '(:publishing-directory "/output/" :output-file "/output/a/index.html")))

  "uri is outside the current-path subdirectory"
  (should-not (org-html-post--in-current-path-p
               "/this.html"
               '(:publishing-directory "/output/" :output-file "/output/a/index.html")))
  (should-not (org-html-post--in-current-path-p
               "/b/c/this.html"
               '(:publishing-directory "/output/" :output-file "/output/a/index.html")))

  "uri is outside the current-path, at the same depth"
  (should-not (org-html-post--in-current-path-p
               "/b/this.html"
               '(:publishing-directory "/output/" :output-file "/output/a/index.html"))))




(ert-deftest org-html-post--project-index-p ()
    :expected-result :failed
    (x-org-html-post--project-index-p))

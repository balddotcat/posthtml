
(require 'package)
(package-initialize)

(load-file "posthtml.el")

(ert-deftest posthtml$--nodes ()
  "creates selector's dom nodes as required"
  (should (string= (funcall (posthtml (posthtml$ [body p strong]))
                            "<html/>")
                   "<html><body><p><strong/></p></body></html>"))
  (should (string= (funcall (posthtml (posthtml$ [body p strong]))
                            "<html><body/></html>")
                   "<html><body><p><strong/></p></body></html>"))
  (should (string= (funcall (posthtml (posthtml$ [body p strong]))
                            "<html><body><p/></body></html>")
                   "<html><body><p><strong/></p></body></html>")))

(ert-deftest posthtml$--args ()
  "accepts contents (string) argument"
  (should (string= (funcall (posthtml (posthtml$ [p strong] "hello"))
                            "<html/>")
                   "<html><p><strong>hello</strong></p></html>"))
  "accepts contents (sxml) argument"
  (should (string= (funcall (posthtml (posthtml$ [p] '(strong () "hello")))
                            "<html/>")
                   "<html><p><strong>hello</strong></p></html>")))

(ert-deftest posthtml$--args:container-position ()
  "accepts first as position argument"
  (should (string= (funcall (posthtml (posthtml$ [head:first]))
                            "<html><body/></html>")
                   "<html><head/><body/></html>")))


(ert-deftest posthtml/doctype ()
  "without arguments, sets default doctype"
  (should (string= (funcall (posthtml (posthtml/doctype)) "<html></html>")
                   "<!DOCTYPE html>\n<html/>"))
  (setf posthtml-doctype '()))

(ert-deftest posthtml/doctype--arguments ()
  "accepts doctype (string) argument"
  (should (string= (funcall (posthtml (posthtml/doctype "<!DOCTYPE xhtml>")) "<html></html>")
                   "<!DOCTYPE xhtml>\n<html/>"))
  (setf posthtml-doctype '()))


(ert-deftest posthtml/head-title ()
  "adds html head title element"
  (should (string= (funcall (posthtml (posthtml/head-title "hello")) "<html/>")
                   "<html><head><title>hello</title></head></html>")))

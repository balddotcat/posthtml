
(load-file "posthtml.el")

(ert-deftest posthtml/doctype ()
  "without arguments, sets default doctype"
  (should (string= (funcall (posthtml (posthtml/doctype)) "<html></html>")
                   "<!DOCTYPE html>\n<html/>"))
  "accepts doctype (string) argument"
  (should (string= (funcall (posthtml (posthtml/doctype "<!DOCTYPE xhtml>")) "<html></html>")
                   "<!DOCTYPE xhtml>\n<html/>")))


(load-file "posthtml.el")

(ert-deftest posthtml-doctype ()
  "sets doctype"
  (should (string= "<!DOCTYPE html>\n<html></html>"
                   (funcall (posthtml (posthtml/doctype)) "<html></html>"))))


# posthtml

`posthtml`  provides  a  fluent interface  to  decorating  a
string  of  HTML,  like  `"<ul><li/><li/></ul>"`  -  to  add
attributes such  as `class`  or `id` to  elements, or  to do
custom processing.

    (posthtml-decorate "<ul><li/><li/></ul>"
    		   '(:id ul "ID")
    		   '(:class each "ul li" "CLASSNAME")
    		   '((lambda (li)
    		       (posthtml-append li "CONTENT"))
    		     "ul>li"))

The string  is parsed into  an `esxml` (DOM)  tree structure
with `libxml-parse-html-region` - the defined decorators are
applied in  sequence, and  a string rendered  via `esxml-to-
xml` is returned.  Querying is done with `esxml-query`.


## decorating HTML output with posthtml-decorate

The first  argument to `posthtml-decorate` is  a function or
one  of the  following tokens;  `prepend`, `append`,  `set`,
`add`, `id` and  `class`. These may be used  with or without
an ':' prefix, and additional functionality can be specified
by updating `posthtml-decorator-token-alist`.

    '(:append h1 " APPEND")         ;; append to contents of h1 element
    '(:set ul :class "COLUMNS")     ;; add COLUMNS class to ul element
    '(:set h1 :id "TWO" :class "")  ;; set h1's id to TWO, remove all classes
    '(:set ul :id "")               ;; remove ul's id attribute

`posthtml-decorate` translates the  syntax decorators into a
call    to   `posthtml-apply`    or   `posthtml-apply-each`,
respectively. They are  responsible for querying `CONTAINER`
for `SELECTOR`,  and applying (each)  found element -  as an
`esxml` parse-tree - and `ARGS` to `FN`.

    '((lambda (list-element current-uri)
        (let ((link-uri (posthtml-attr (posthtml-find list-element 'a) 'href)))
          (when (string= link-uri current-uri)
    	(posthtml-set list-element :class "CURRENT"))))
      each "ul li" "/uri-two")

As the element provided to each function is an `esxml` parse
-tree,   it   is   available  for   further   querying   and
processing. In  the following,  the `ul`  is updated  with a
dynamically created  `CSS` class  name, to reflect  how many
elements it contains.

    '((lambda (ul)
        (posthtml-add ul :class
    		  (format "COLUMNSWIDTH%s"
    			  (length (posthtml-find-all ul "li")))))
      "ul"))

Here is the result of running this contrived test case, with
input on the left.

    <html><body>                                | <html><body>
        <h1 class=\"ONE\">headline</h1>         |     <h1 id=\"TWO\">headline APPEND</h1>
        <ul id=\"ONE\">                         |     <ul class=\"COLUMNS COLUMNSWIDTH3\">
          <li><a href="/uri-one">uri</a></li>   |       <li><a href="/uri-one">uri</a></li>
          <li><a href="/uri-two">uri</a></li>   |       <li class="CURRENT"><a href="/uri-two">uri</a></li>
          <li><a href="/uri-three">uri</a></li> |       <li><a href="/uri-three">uri</a></li>
        </ul>                                   |     </ul>
    </body></html>                              | </body></html>


### selecting DOM nodes

`posthtml-find` and `posthtml-find-all` both utilize [esxml-
query](https://github.com/tali713/esxml)      to     provide
`querySelector`-like  functionality in  finding (collections
of) nodes with CSS selectors.

Selectors are passed in as a string, as a combination of the
following available options.

    foo, bar       ;; Commas
    foo bar        ;; Descendant combinator
    foo>bar        ;; Child combinator
    *              ;; Universal selector
    tag            ;; Type selector
    #foo           ;; ID selector
    .foo           ;; Class selector
    [foo]          ;; Attribute selector
    [foo=bar]      ;; Exact match attribute selector
    [foo^=bar]     ;; Prefix match attribute selector
    [foo$=bar]     ;; Suffix match attribute selector
    [foo*=bar]     ;; Substring match attribute selector
    [foo~=bar]     ;; Include match attribute selector
    [foo|bar]      ;; Dash match attribute selector


### dom manipulation

`posthtml-append` and `posthtml-prepend`  add child nodes to
elements;  the  new  elements  can  be  strings  or  `esxml`
lists.  Please   also  see  the  documentation   for  Emacs'
`Document Object Model` package.

[dom.el](https://www.gnu.org/software/emacs/manual/html_node/elisp/Document-Object-Model.html)


### attributes

`posthtml-attr`  retrieves,  or with  a  value  sets, a  dom
node's attribute.

Supplying  an empty  string  to  `posthtml-set` removes  the
attribute, `posthtml-add` appends to the current value.


## example usage: tweaking org-export output

`org-export-filter-final-output-functions` provide access to
the final  output of  any `org-export`,  or `org-publishing`
sequence; this  list consists  of the last  functions called
with the rendered output,  their output being what's written
to disk.

    (add-to-list 'org-export-filter-final-output-functions
    	     (lambda (contents backend info)
    	       (posthtml-decorate
    		contents
    		'(:set body :style "background-color: black;"))))


## tests

    emacs -batch \
          -l ert \
          -l posthtml-tests.el \
          -f ert-run-tests-batch-and-exit

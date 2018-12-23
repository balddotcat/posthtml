

# posthtml

`posthtml` provides a fluent interface to decorating a string of HTML like
`"<ul><li/><li/></ul>"` - to add attributes such as `class` or `id` to elements,
or to do custom processing.

    (posthtml-decorate "<ul><li/><li/></ul>"
                       '($ [ul] :attr :id 'CONTAINER)
                       '($each [ul li] :attr :class 'ELEMENT))

The string is parsed into an `esxml` representation of the DOM tree by
`libxml-parse-html-region` - the defined decorators are applied, and a string
rendered via `esxml-to-xml` is returned. Querying is done with `enlive`.

As a typical use case scenario, `org-export-filter-final-output-functions` provide
access to the final output of any `org-export`, or `org-publishing` sequence; this
list consists of the last functions called with the rendered output, their output
being what's written to disk.

    (add-to-list 'org-export-filter-final-output-functions
                 (lambda (contents backend info)
                   (posthtml-decorate contents
                                      '($ [body] :attr :style "background-color: black;"))))


-   [github.com/balddotcat/posthtml](https://github.com/balddotcat/posthtml)
-   [bald.cat/posthtml](http://bald.cat/posthtml)


## posthtml-decorate `string &rest decorators`

    '($ [h1] :append " APPEND")             ; append to contents of h1 element
    '($ [ul] :attr :class 'COLUMNS)         ; add COLUMNS class to ul element
    '($ [h1] :attrs-set :id 'TWO :class "") ; set h1's id to TWO, remove all classes
    '($ [ul] :attr-set :id "")              ; remove ul's id attribute

`posthtml-decorate` translates the syntax decorators into a calls to `posthtml-apply` or
`posthtml-apply-each`, respectively. They are responsible for querying `CONTAINER`
for `SELECTOR`, and applying (each) found element - as an `esxml` parse-tree - and
`ARGS` to `FN`.

    '($each [ul li]
            (lambda (list-element current-uri)
              (let ((link-uri (posthtml-attribute (posthtml-find list-element [a]) 'href)))
                (when (string= link-uri current-uri)
                  (posthtml-attribute list-element :class 'CURRENT))))
            "/uri-two")

As the element provided to each function is an `esxml` parse-tree, it is available
for further querying and processing. In the following, the `ul` is updated with a
dynamically created `CSS` class name, to reflect how many elements it contains.

    '($ [ul]
        (lambda (ul)
          (posthtml-attribute ul :class
                              (format "COLUMNSWIDTH%s"
                                      (length
                                       (posthtml-find-all ul [li]))))))))

Here is the result of running this contrived test case, with input on the left.

    <html><body>                                | <html><body>
        <h1 class=\"ONE\">headline</h1>         |     <h1 id=\"TWO\">headline APPEND</h1>
        <ul id=\"ONE\">                         |     <ul class=\"COLUMNS COLUMNSWIDTH3\">
          <li><a href="/uri-one">uri</a></li>   |       <li><a href="/uri-one">uri</a></li>
          <li><a href="/uri-two">uri</a></li>   |       <li class="CURRENT"><a href="/uri-two">uri</a></li>
          <li><a href="/uri-three">uri</a></li> |       <li><a href="/uri-three">uri</a></li>
        </ul>                                   |     </ul>
    </body></html>                              | </body></html>


### syntax decorators

-   **`$`:** posthtml-apply
-   **`$each`:** posthtml-apply-each
-   **`lambda`:** called with `esxml` parse-tree, and optional `args`
-   **`fn`:** called with `esxml` parse-tree, and optional `args`


#### actions

-   **`:append`:** posthtml-append
-   **`:prepend`:** posthtml-prepend
-   **`:attr`:** posthtml-attribute
-   **`:attr-set`:** posthtml-attributes-set
-   **`:attrs`:** posthtml-attributes
-   **`:attrs-set`:** posthtml-attributes-set


## posthtml-find `esxml selector`

Wrapper function to [enlive-query](https://github.com/tali713/esxml/blob/master/esxml-query.el) - please see source for full list of **selector** examples.


### posthtml-findall `esxml selector`

**Selectors** are `vector` objects, resembling **CSS** selectors which can have `id`-s, `classes`, `attributes` - as per **enlive**.


## posthtml-append `esxml element`

Element can be a string or an `esxml` element.


## posthtml-prepend `esxml element`

Element can be a string or an `esxml` element.


## posthtml-attribute `esxml attribute &optional value force`

Returns the value of an `esxml` element's attribute. Optionally, when provided with
a `value`, set or add `attribute` to element - set as only property with `force`.

-   attributes can start with or without an initial `:`


### posthtml-attributes `esxml &rest attributes`


### posthtml-attributes-set `esxml &rest attributes`


## tests

    emacs -batch \
          -l ert \
          -l posthtml-tests.el \
          -f ert-run-tests-batch-and-exit

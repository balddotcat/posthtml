

# posthtml

**posthtml** is an **emacs org-mode export filter** for cosmetic touchups of `HTML`
output; a macro that gives access to the `DOM` structure, allowing for
querying and modification with `CSS`-like selectors.

-   [github.com/balddotcat/posthtml](https://github.com/balddotcat/posthtml)
-   [bald.cat/posthtml](http://bald.cat/posthtml)

It takes an `HTML` string, or the final `HTML` output of an `org mode`
export/publishing action, and works with an
[esxml](https://github.com/tali713/esxml) representation of the DOM tree, as
parsed by `libxml-parse-html-region`; returns a string via `esxml-to-xml`.  Querying
is done with [enlive](https://github.com/zweifisch/enlive).

    ($each [header nav li]
           (lambda (nav-element uri)
             (let ((href (posthtml$ nav-element [a] :attr 'href)))
               (when (and (not (string= "/" href))
                          (string-prefix-p href uri t))
                 (posthtml-attribute nav-element 'class 'current))))
           (page-uri info))


## macros

    (posthtml:filter-final-output [...])

    (posthtml:filter content options [...])


## selectors

**posthtml$ (container selector &optional fn &rest args)**

    (posthtml$ esxml-container [html] 'posthtml-append "this")
    ($ [html] :append "this")
    ($ [html] (lambda (container this) (posthtml-append container this)) "this")

**posthtml$each (container selector &optional fn &rest args)**

    (posthtml$each esxml-container [body p] 'posthtml-append "this")
    ($each [body p] :append "this")
    ($each [body p] (lambda (element this) (posthtml-append element this)) "this")

Query `CONTAINER` for `SELECTOR`; apply (each) found element and `ARGS` to `FN` -
the following shorthands are available for `FN`;

-   `:append` for 'posthtml-append
-   `:prepend` for 'posthtml-prepend
-   `:attr` for 'posthtml-attribute"


## dom manipulation

-   posthtml-append (container &optional element)
-   posthtml-prepend (container &optional element)


### attributes

-   posthtml-attribute (element attribute &optional values)
-   posthtml-attribute-set (element attribute &optional values)


## tests

    emacs -batch \
          -l ert \
          -l posthtml-tests.el \
          -f ert-run-tests-batch-and-exit

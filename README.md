**posthtml** is an **emacs org-mode export filter** for cosmetic touchups of `HTML` output; a set of macros that give access to the `DOM` structure, allowing for querying and modification with `CSS`-like selectors.

    ($each [header nav li]
           (lambda (nav-element uri)
             (let ((href (posthtml$ nav-element [a] :attr 'href)))
               (when (and (not (string= "/" href))
                          (string-prefix-p href uri t))
                 (posthtml-attribute nav-element 'class 'current))))
           (page-uri info))


**posthtml** takes the final `HTML` output of an `org mode` export/publishing action; it works with an [esxml](https://github.com/tali713/esxml) representation of the DOM tree, as parsed by `libxml-parse-html-region`; it returns a string via `esxml-to-xml`. Querying is done with [enlive](https://github.com/zweifisch/enlive).



# functions


## posthtml$each (container selector &optional fn &rest args)

    (posthtml$each [body p] :append "this")
    ($each [body p] :append "this")

Query CONTAINER for SELECTOR; apply each found element and ARGS to FN, return results. CONTAINER is an `esxml list`, SELECTOR is a `vector`; ie [html body].

The following shorthands are available for FN; `:append` for 'posthtml-append, `:prepend` for 'posthtml-prepend, `:attr` for 'posthtml-attribute.


## posthtml$ (container selector &optional fn &rest args)

    (posthtml$ [html] :append "this")
    ($ [html] :append "this")

Query CONTAINER for SELECTOR; apply found element and ARGS to FN, return results. CONTAINER is an `esxml list`, SELECTOR is a `vector`; ie [html body].

The following shorthands are available for FN; `:append` for 'posthtml-append, `:prepend` for 'posthtml-prepend, `:attr` for 'posthtml-attribute


## posthtml-prepend (container &optional element)

Prepend ELEMENT to CONTAINER. CONTAINER is an `esxml list`, ELEMENT can be a `list` or a `string`.


## posthtml-append (container &optional element)

Append ELEMENT to CONTAINER. CONTAINER is an `esxml list`, ELEMENT can be a `list` or a `string`.


## posthtml-attribute (element attribute &optional values)

Return ELEMENT ATTRIBUTE; with VALUES argument, add ATTRIBUTE with VALUE to ELEMENT. ELEMENT is an `esxml list`, ATTRIBUTE and VALUES are `strings`.



# setup


## org export (derived) backend

    (org-export-define-derived-backend
     'test1 'html
     :filters-alist `((:filter-final-output
                       ,(posthtml (setq contents "THIS")))))


## default export process

    (posthtml-filter-export-output (setq contents "THIS"))


## org publishing process :preparation-action

    (add-to-list
     'org-publish-project-alist
     `("project"
       :preparation-function (,(posthtml-add-export-filter
                                (setq contents "THIS")))))



# tests

    emacs -batch \
          -l ert \
          -l posthtml-tests.el \
          -f ert-run-tests-batch-and-exit

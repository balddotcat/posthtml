**[org-html-post](http://manifold.io/project/org-html-post)** defines post rendering hooks for **org-export**; it works with a `parse-tree` representation of the current page, providing programmatic access to it's DOM.

-   [source available on github](https://github.com/elolaszlo/org-html-post)

# setup

    (load "~/lisp/org-html-post.el")

    (org-html-post-add-filter
     '(when (s-starts-with "/blog" (org-html-post--page-uri info))
        (org-html-post-add-attribute (enlive-query contents [body])
                                     'class 'dark-theme)))

# usage

**org-html-post** sets up chains of filters that are applied to the end of an **org export** or **publishing** process - they are executed once for each document.

The chain is essentially the BODY of an `org-export-filter-final-output-function`. It works with an **[esxml](https://github.com/tali713/esxml)** representation of the current output, CONTENTS; it also has access to the current INFO object.

Elements can be queried for with CSS-like selectors using **[enlive-query](https://github.com/zweifisch/enlive)** - the overall DOM structure is simple HTML, and can be edited as expected. For a quick usage overview and a sample configuration, please see [web publishing with org-mode](http://manifold.io/blog/web-publishing-with-org-mode).

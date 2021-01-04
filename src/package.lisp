;;;; package.lisp

(defpackage #:alists
  (:use #:cl #:alexandria)
  (:export #:aconses
           #:deep-acons
           #:merge-acons))

(defpackage #:hm
  (:use #:cl)
  (:shadow #:get
           #:reduce
           #:first)
  (:export #:put
           #:get
           #:one
           #:reduce
           #:print-elt
           #:print-all))

(defpackage #:mop
  (:use #:cl #:alexandria)
  (:export #:make-mapper
           #:find-class-slots
           #:class-slots
           #:defprintobj
           #:with-computed-slot
           #:with-mapped-slot
           #:with-renamed-slot))

(defpackage #:opti-shopylist
  (:use #:cl #:lquery)
  (:export #:extract-constitution-1958))

(defpackage #:jsons
  (:use #:cl)
  (:export #:get-in
           #:add-value
           #:type-compatible-p))

(defpackage #:resources
  (:use #:cl)
  (:export #:*profile*
           #:*system*
           #:resource))

(defpackage #:api
  (:use #:cl #:snooze #:jsons #:alexandria #:resources)
  (:export #:start
           #:stop))

(defpackage #:db
  (:use #:cl)
  (:export #:create-shopping-item
           #:get-shopping-item
           #:set-bought-item
           #:delete-shopping-item
           #:get-all
           #:product-name
           #:status
           #:connect))

(defpackage #:html
  (:use #:cl #:spinneret #:alexandria)
  (:export #:secret-login
           #:shopping-list))

(defpackage #:web-site
  (:use #:cl #:snooze #:jsons #:alexandria)
  (:export #:shopping-list
           #:shopping-item-name
           #:shopping-item-timestamp
           #:shopping-item-bought
           #:shopping-item
           #:*set-not-bought*
           #:*set-bought*
           #:*add-shopping-item*
           #:*remove-shopping-item*))

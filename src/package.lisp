;;;; package.lisp

(defpackage #:macro-utils
  (:use #:cl)
  (:export #:mkstr
           #:symb
           #:kw
           #:group
           #:flatit
           #:fact
           #:choose))

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

(defpackage #:models
  (:use #:cl)
  (:export #:shopping-item-name
           #:shopping-item-timestamp
           #:shopping-item-bought
           #:shopping-item
           #:make-shopping-item))

(defpackage #:shopping-interface
  (:use #:cl)
  (:export #:add-shopping-item
           #:remove-shopping-item
           #:set-item-bought
           #:get-all))

(defpackage #:shopping-mem-impl
  (:use #:cl #:alexandria #:shopping-interface #:models)
  (:export #:add-shopping-item
           #:remove-shopping-item
           #:set-item-bought
           #:get-all))

(defpackage #:shopping-db-impl
  (:use #:cl #:alexandria #:shopping-interface #:models)
  (:export #:add-shopping-item
           #:remove-shopping-item
           #:set-item-bought
           #:get-all))

(defpackage #:html
  (:use #:cl #:spinneret #:alexandria #:models)
  (:export #:secret-login
           #:shopping-list))

(defpackage #:web-site
  (:use #:cl
        #:snooze
        #:jsons
        #:alexandria
        #:models
        #:shopping-interface)
  (:export #:shopping-list
           #:make-shopping-list-manager))

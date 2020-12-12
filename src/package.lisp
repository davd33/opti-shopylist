;;;; package.lisp

(defpackage #:alists
  (:use #:cl #:alexandria)
  (:export #:aconses
           #:deep-acons
           #:merge-acons))

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

(defpackage #:html
  (:use #:cl #:spinneret #:alexandria)
  (:export #:secret-login))

(defpackage #:web-site
  (:use #:cl #:snooze #:jsons #:alexandria)
  (:export #:shopping-list))

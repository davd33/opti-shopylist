;;;; opti-shopylist.asd

(asdf:defsystem #:opti-shopylist
  :description "Manage a shopping list with your opti-friends."
  :author "David Rueda <davd33@gmail.com>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:spinneret
               #:hunchentoot
               #:snooze
               #:quri
               #:dexador
               #:lquery
               #:cl-ppcre
               #:cl-json
               #:clack
               #:fset
               #:str
               #:mito
               #:sxql
               #:unix-opts
               #:trivia
               #:alexandria
               #:closer-mop)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "resources")
                 (:file "mop")
                 (:file "jsons")
                 (:file "opti-shopylist")
                 (:file "api")
                 (:file "html")
                 (:file "web-site"))))
  :in-order-to ((test-op (test-op "opti-shopylist/tests"))))

(asdf:defsystem "opti-shopylist/tests"
  :author "David Rueda"
  :licence "GPLv3"
  :depends-on ("opti-shopylist"
               "rove")
  :description "Test system for opti-shopylist"
  :components ((:module "tests"
                                        ;:components
                                        ;(())
                        ))
  :perform (test-op (op c) (symbol-call :rove :run c)))

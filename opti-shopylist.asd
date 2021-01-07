;;;; opti-shopylist.asd

(asdf:defsystem #:opti-shopylist
  :description "Manage a shopping list with your opti-friends."
  :author "David Rueda <davd33@gmail.com>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (;; HTML / HTTP Routes
               #:spinneret
               #:hunchentoot
               #:snooze
               #:clack
               ;; URI deserialize
               #:quri
               ;; Make HTTP requests
               #:dexador
               ;; CL jQuery for HTML strings
               #:lquery
               ;; Basics
               #:cl-ppcre               ; Regex
               #:cl-json
               #:fset                   ; Functional data structures
               #:str
               #:trivia                 ; Pattern matching
               #:alexandria
               ;; Database
               #:mito
               #:sxql
               ;; Async
               #:lparallel
               ;; Programs options
               #:unix-opts
               ;; OOP extensions
               #:closer-mop
               ;; JWT Generation
               #:cljwt-custom
               ;;#:ironclad               ; Crypto functions
               ;;#:cl-base64              ; (Part of CL)
               ;;#:flexi-stream           ; Bivalent streams
               )
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "hm")
                 (:file "resources")
                 ;;(:file "mop")
                 ;;(:file "jsons") ; needs mop
                 (:file "opti-shopylist")
                 (:file "api")
                 (:file "db")
                 (:file "models")
                 (:file "shopping-interface")
                 (:file "shopping-mem-impl")
                 (:file "shopping-db-impl")
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
                :components
                ((:file "make-shopping-item-manager"))))
  :perform (test-op (op c) (symbol-call :rove :run c)))

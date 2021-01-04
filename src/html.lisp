(in-package :html)

(defparameter *page-title* "Opti-shopylist")

(defmacro css (&body styles)
  "Takes 1..n CSS instructions as 2-elements lists, then returns a css formatted string.
   A CSS instruction list looks like this: (:font-size <string>)"
  `(str:concat
     ,@(loop for style in styles
          collect `(format nil "~a: ~a;~%" ,(string (first style)) ,(second style)))))

(defmacro with-page ((&key title image-path) &body body)
  `(spinneret:with-html
     (:doctype)
     (:html
      (:head
       (:link :href "/css/main.css" :rel "stylesheet" :type "text/css")
       (:title ,title)
       (when ,image-path
         (:style "html {"
                 (css (:background (str:concat "url(" ,image-path ")")))
                 "background-size: cover;"
                 "background-repeat: no-repeat;"
                 "}")))
      (:body
       (:div.container ,@body)))))

(deftag link (text attrs &key href class)
  `(:a.contact-link
    :class ,class
    :href ,href
    ,@attrs
    ,@text))

(deftag repeat (template attrs &key for)
  "This is a tag that repeats a given template using the key
for a translation split into a list of several strings.
  - for: lang-binding-form: 2 elements list with var name and translation key
  - template: a single form (one list of potentially embedded tags)"
  `(reduce #'(lambda (acc elt)
               (append
                acc
                (let ((,(caadr for) elt))
                  ,@template)))
           ,@(cdadr for)
           :initial-value `(progn)))

(defun secret-login (login-error)
  "There is a common password for all optimums.
One solely password for each instance of the website."
  (with-page (:title *page-title* :image-path nil)
    (:div
     :id "login"
     (:div
      :class "login-form"
      (:p
       :class (if login-error "error-msg" "info-msg")
       (or login-error "Please give the password:"))
      (:form
       :method "POST"
       (:input
        :type "password"
        :name "password"
        :placeholder "...here")
       (:input
        :type "submit"
        :value "Go"))))
    ))

(defun shopping-list (signout-path shopping-list)
  "Manage the shopping list and log out."
  (with-page (:title *page-title* :image-path nil)
    (:div
     (:div
      :id "signout"
      (:p
       "You're logged-in, but you may well "
       (:a :href signout-path
           "sign out")
       " too! (if you want)"))
     (:div
      :class "shopping-list-container"
      (:div
       :class "shopping-list-add"
       (:form
        :method "POST"
        (:input :type "text"
                :name "product-name"
                :placeholder "What..."
                :autocomplete "off"
                :required
                :autofocus)
        (:input :type "hidden"
                :name "action"
                :value :add-shopping-item)
        (:input :type "submit"
                :value "Add")))
      (:ul
       :class "shopping-list"
       (dolist (item shopping-list)
         (:li
          :class "item"
          (:form :method "POST"
                 (:input :type "hidden"
                         :name "product-name"
                         :value (web-site:shopping-item-name item))
                 (:input :type "hidden"
                         :name "action"
                         :value :remove-shopping-item)
                 (:input :type "submit" :value "☠"))
          (:form :method "POST"
                 (:input :type "hidden"
                         :name "product-name"
                         :value (web-site:shopping-item-name item))
                 (:input :type "hidden"
                         :name "action"
                         :value (if (web-site:shopping-item-bought item)
                                    :set-not-bought
                                    :set-bought))
                 (:input :type "submit" :value (if (web-site:shopping-item-bought item)
                                                   "☑"
                                                   "☐"))
                 (:input :type "submit" :class "shopping-item-text" :value (web-site:shopping-item-name item))))))))))

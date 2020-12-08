(in-package #:web-site)

(defmacro build-spinneret-html-response (&body body)
  `(with-output-to-string (out)
     (let ((spinneret:*html* out))
       ,@body)))

(define-condition redirect (snooze:http-condition)
  ((location :initarg :location :accessor location))
  (:default-initargs :status-code 303))

;;; HOME OF WEBSITE

(defvar shopping-list '())
(defvar connected-users '())

(defroute shopping-list
  (:get "text/html")
  "The shopping list")

(defgenpath shopping-list shopping-list-path)

(defroute secret-login
  (:post "application/x-www-form-urlencoded")
  (let* ((payload (payload-as-string))
         (password (second (str:split "=" payload))))
    (signal 'redirect :location (shopping-list-path))))

(defroute secret-login
  (:get "text/html")
  (build-spinneret-html-response
    (html:secret-login)))

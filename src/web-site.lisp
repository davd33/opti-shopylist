(in-package #:web-site)

(defmacro build-spinneret-html-response (&body body)
  `(with-output-to-string (out)
     (let ((spinneret:*html* out))
       ,@body)))

(define-condition redirect (snooze:http-condition)
  ((location :initarg :location :accessor location))
  (:default-initargs :status-code 303))

(defun redirect (location-path)
  (signal 'redirect :status-code 303 :location location-path))

;;; HOME OF WEBSITE

(defstruct connected-user
  token expire-time)

(defvar shopping-list '())

(defvar connected-users '()
  "Every CONNECTED-USER of the shopping list.")

(defresource shopping-list (verb ct) (:genpath shopping-list-path))

(defroute shopping-list
  (:get "text/html")
  "The shopping list")

(defroute secret-login
  (:post "application/x-www-form-urlencoded")
  (let* ((payload (payload-as-string))
         (password (second (str:split "=" payload))))
    (hunchentoot:set-cookie "TOKEN-OPTIMUM"
                            :value (str:concat "my-token-and-my-password"
                                               password)
                            :expires (+ 60 (get-universal-time)))
    (redirect (shopping-list-path))))

(defmethod explain-condition ((c redirect) rs ct)
  (declare (ignore rs ct))
  (setf (hunchentoot:header-out :location) (location c))
  (format nil "See here: ~a" (location c)))

(defroute secret-login
  (:get "text/html")
  (build-spinneret-html-response
    (html:secret-login)))

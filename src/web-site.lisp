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

(defparameter *opti-password* "opti-password")

(defvar shopping-list '())

(defvar connected-users '()
  "Every activated JWT of the shopping list.")

;; --- SHOPPING LIST

(defresource shopping-list (verb ct) (:genpath shopping-list-path))

(defroute shopping-list
  (:get "text/html")
  "The shopping list")

;; --- SECRET LOGIN

(defresource secret-login (verb ct &key login-error) (:genpath secret-login-path))

(defroute secret-login
  (:get "text/html" &key login-error)
  (build-spinneret-html-response
    (html:secret-login login-error)))

(defroute secret-login
  (:post "application/x-www-form-urlencoded" &key login-error)
  (declare (ignore login-error))
  (let* ((payload (payload-as-string))
         (password (second (str:split "=" payload)))
         (my-token (when (string= password *opti-password*)
                     "opti-token")) ; todo generate JWT
         )

    (if (null my-token)
        ;; password is wrong
        (redirect (secret-login-path :login-error "We gotcha mother fucker! Haha Sleeping ass."))
        ;; login succeeded
        (progn
          (hunchentoot:set-cookie "TOKEN-OPTIMUM"
                                  :value my-token
                                  :expires (+ 60 (get-universal-time)))
          (redirect (shopping-list-path))))))

(defmethod explain-condition ((c redirect) rs ct)
  (declare (ignore rs ct))
  (setf (hunchentoot:header-out :location) (location c))
  (format nil "See here: ~a" (location c)))

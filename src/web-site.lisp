(in-package #:web-site)

;;; --- UTILS

(defmacro build-spinneret-html-response (&body body)
  `(with-output-to-string (out)
     (let ((spinneret:*html* out))
       ,@body)))

(defun gen-token ()
  "Generate a new user token."
  (str:concat *token-cookie-name* "_" (write-to-string (get-universal-time))))

;;; --- REDIRECTS

(define-condition redirect (snooze:http-condition)
  ((location :initarg :location :accessor location))
  (:default-initargs :status-code 303))

(defun redirect (location-path)
  "Signals a REDIRECT condition with 303 HTTP status code."
  (signal 'redirect :status-code 303 :location location-path))

(defmethod explain-condition ((c redirect) rs ct)
  "Sets the redirection location as a header in Hunchentoot."
  (declare (ignore rs ct))
  (setf (hunchentoot:header-out :location) (location c))
  (format nil "See here: ~a" (location c)))

;;; --- CONFIGURATION

(defparameter *token-cookie-name* "OPTI-TOKEN"
  "The name of the cookie used to identified a logged-in user.")

(defparameter *opti-password* (or (uiop:getenv "OPTIPASSWD") "OPTI-PASSWORD")
  "Password for accessing the shopping list.")

(defvar *shopping-list* '()
  "List of items to buy.")

(defvar *connected-users* '()
  "Every CONNECTED-USER of the shopping list.")

(defparameter *set-not-bought* "SET_NOT_BOUGHT")

(defparameter *set-bought* "SET_BOUGHT")

(defparameter *add-shopping-item* "ADD_SHOPPING_ITEM")

;;; --- TYPES

(defstruct shopping-item
  "An item of the shopping list - something to buy or already bought."
  timestamp
  name
  bought)

(defstruct (connected-user (:constructor mk-connected-user
                               (token expire-time)))
  "A connected user represents someone of us that has used the right
*OPTI-PASSWORD* as SECRET-LOGIN and is thus registred in the
*CONNECTED-USERS* list."
  token
  expire-time)

;;; --- FUNCTIONAL FEATURES

(defun shopping-item< (item1 item2)
  "Does ITEM1 have more visibility than ITEM2?"
  (let ((bought1 (shopping-item-bought item1))
        (bought2 (shopping-item-bought item2))
        (time1 (shopping-item-timestamp item1))
        (time2 (shopping-item-timestamp item2)))
    (cond ((and bought1 bought2)
           (> time1 time2))
          ((and (null bought1) (null bought2))
           (> time1 time2))
          (t
           (and (null bought1) bought2)))))

;;; --- DEFINE RESOURCES

(defresource shopping-list (verb ct) (:genpath shopping-list-path))

(defresource secret-login (verb ct &key login-error) (:genpath secret-login-path))

(defresource sign-out (verb ct &key user-token) (:genpath signout-path))

;;; --- SIGN OUTgoing

(defroute sign-out
  (:get "text/html" &key user-token)

  (setf *connected-users*
        (remove-if (lambda (cuser)
                     (string= (connected-user-token cuser) user-token))
                   *connected-users*))

  (hunchentoot:set-cookie *token-cookie-name*
                          :value nil
                          :expires 0
                          :max-age 0
                          :path "/")

  (redirect (secret-login-path)))

;;; --- SHOPPING LIST

(defroute shopping-list
  (:post "application/x-www-form-urlencoded")
  (let* ((payload (quri:url-decode-params (payload-as-string)))
         (product-name (cdr (assoc "product-name" payload :test #'string=)))
         (item (make-shopping-item :name product-name
                                   :timestamp (get-universal-time)))
         (action (cdr (assoc "action" payload :test #'string=))))

    (cond ((string= *add-shopping-item* action)
           (setf *shopping-list*
                 (adjoin item *shopping-list* :key (compose #'str:upcase #'shopping-item-name) :test #'string=)))
          ((or (string= *set-not-bought* action) (string= *set-bought* action))
           (let ((item (find product-name *shopping-list* :key #'shopping-item-name :test #'string=)))
             (setf (shopping-item-bought item)
                   (if (string= *set-not-bought* action)
                       nil
                       t)))))

    (setf *shopping-list*
          (stable-sort *shopping-list* #'shopping-item<))

    (redirect (shopping-list-path))))

(defroute shopping-list
  (:get "text/html")
  (let* ((req-token (hunchentoot:cookie-in *token-cookie-name*))
         (found-cuser (find req-token *connected-users*
                            :test #'string=
                            :key
                            (lambda (cuser) (connected-user-token cuser)))))

    ;; Check whether the user is correctly logged in
    (cond ((null req-token)
           (redirect (secret-login-path :login-error "You are not yet logged in.")))
          ((not found-cuser)
           (redirect (secret-login-path :login-error "Hacker? Better log in!")))
          ((> (get-universal-time) (connected-user-expire-time found-cuser))
           (redirect (secret-login-path :login-error "Your session has expired!"))))

    (build-spinneret-html-response
      (html:shopping-list (signout-path :user-token req-token)
                          *shopping-list*))))

;;; --- SECRET LOGIN

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
                     (gen-token))) ; todo generate JWT
         (expire-time (* 2 (+ 60 (get-universal-time))))
         )

    (if (null my-token)
        ;; password is wrong
        (redirect (secret-login-path :login-error "We gotcha mother fucker! Haha Sleeping ass."))
        ;; login succeeded
        (progn
          (setf *connected-users*
                (cons (mk-connected-user my-token expire-time)
                      *connected-users*))

          (hunchentoot:set-cookie *token-cookie-name*
                                  :value my-token
                                  :expires expire-time
                                  :path "/")

          (redirect (shopping-list-path))))))

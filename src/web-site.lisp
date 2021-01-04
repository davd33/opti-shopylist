(in-package #:web-site)

;;; --- UTILS

(defmacro build-spinneret-html-response (&body body)
  `(with-output-to-string (out)
     (let ((spinneret:*html* out))
       ,@body)))

(defun gen-token ()
  "Generate a new user token."
  (str:concat *token-cookie-name* "_" (write-to-string (get-universal-time))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun kw (&rest args)
  (values (intern (apply #'mkstr args) :keyword)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                               (subseq source 0 n)
                               acc))
                   (nreverse
                     (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                        (car x)
                        (rec (cdr x) acc))))))
    (rec x nil)))

(defun fact (x)
  (if (= x 0)
    1
    (* x (fact (- x 1)))))

(defun choose (n r)
  (/ (fact n)
     (fact (- n r))
     (fact r)))

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

(defgeneric add-shopping-item (kind item shopping-list)
  (:documentation "Add an ITEM to the SHOPPING-LIST."))

(defgeneric remove-shopping-item (kind item shopping-list)
  (:documentation "Remove an ITEM from the SHOPPING-LIST."))

(defgeneric set-item-bought (kind item shopping-list bought-p)
  (:documentation "Toggle bought flag of ITEM in SHOPPING-LIST."))

(defmethod add-shopping-item ((kind (eql :IN_MEMORY)) (item shopping-item) shopping-list)
  (adjoin item shopping-list
          :key (compose #'str:upcase #'shopping-item-name) :test #'string=))

(defmethod remove-shopping-item ((kind (eql :IN_MEMORY)) (item shopping-item) shopping-list)
  (remove (shopping-item-name item) shopping-list
          :test #'string=
          :key #'shopping-item-name))

(defmethod set-item-bought ((kind (eql :IN_MEMORY)) (item shopping-item) shopping-list bought-p)
  (substitute (make-shopping-item :timestamp (get-universal-time)
                                  :name (shopping-item-name item)
                                  :bought bought-p)
              item
              shopping-list
              :test (lambda (a b)
                      (string= (shopping-item-name a)
                               (shopping-item-name b)))))

(defun make-shopping-list-manager (kind)
  "Creates a new shopping-list.
Returns a function that takes an ACTION and an ITEM and
calls the appropriate function."
  (let (shopping-list)
    (lambda (action &optional item)
      (ecase action

        (:|GET-SHOPPING-LIST|
         shopping-list)

        (:|SORT-SHOPPING-LIST|
         (setf shopping-list
               (stable-sort shopping-list #'shopping-item<)))

        (:|ADD-SHOPPING-ITEM|
         (setf shopping-list
               (add-shopping-item kind item shopping-list)))

        (:|REMOVE-SHOPPING-ITEM|
         (setf shopping-list
               (remove-shopping-item kind item shopping-list)))

        ((:|SET-NOT-BOUGHT| :|SET-BOUGHT|)
         (setf shopping-list
               (set-item-bought kind item shopping-list (eq action :|SET-BOUGHT|))))))))

;;; --- CONFIGURATION

(defparameter *token-cookie-name* "OPTI-TOKEN"
  "The name of the cookie used to identified a logged-in user.")

(defparameter *opti-password* (or (uiop:getenv "OPTIPASSWD") "OPTI-PASSWORD")
  "Password for accessing the shopping list.")

(defvar *shopping-list-manager* (make-shopping-list-manager
                                 (or (uiop:getenv "SHOPPING_LIST_MANAGER")
                                     :IN_MEMORY))
  "Shopping list manager (can be a IN_MEMORY or DATABASE one).")

(defvar *connected-users* '()
  "Every CONNECTED-USER of the shopping list.")

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

    (funcall *shopping-list-manager* (kw (str:upcase action)) item)

    (funcall *shopping-list-manager* :sort-shopping-list)

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
                          (funcall *shopping-list-manager* :|GET-SHOPPING-LIST|)))))

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

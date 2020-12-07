(in-package #:web-site)

(defmacro build-spinneret-html-response (&body body)
  `(with-output-to-string (out)
     (let ((spinneret:*html* out))
       ,@body)))

(defvar *constitution-1958* nil)

;;; HOME OF WEBSITE

(defroute secret-login
  (:post "application/x-www-form-urlencoded")
  (let ((payload (payload-as-string)))
    (format t "~%WE CALLED POST~%~%" )
    (format t "~%~A~%" payload))
  "hello world")

(defroute secret-login
  (:get "text/html")
  (build-spinneret-html-response
    (html:secret-login)))

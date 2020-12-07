(in-package #:api)

(setf snooze:*catch-errors* :verbose)

;; START HTTP SERVER
(defclass snooze-acceptor (hunchentoot:easy-acceptor) ())

(defparameter *lispdoc-dispatch-table*
  (list
   (hunchentoot:create-folder-dispatcher-and-handler
    "/images/" (fad:pathname-as-directory (resource "images")))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/css/" (fad:pathname-as-directory (resource "css")))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/webfonts/" (fad:pathname-as-directory (resource "webfonts")))
   (make-hunchentoot-app '((*home-resource* . web-site:home)))))

(defmethod hunchentoot:acceptor-dispatch-request :around ((a snooze-acceptor) request)
  (let ((hunchentoot:*dispatch-table* *lispdoc-dispatch-table*))
    (call-next-method)))

(defvar *server* nil)

(defun stop ()
  (when *server* (hunchentoot:stop *server*) (setq *server* nil)))

(defun start (&key (port 5000))
  "Start the HTTP server"
  (stop)
  (setq *server*
        (hunchentoot:start (make-instance 'snooze-acceptor :port port)))
  (format t "~%Program started: Open now the following URL in your web browser: http://localhost:~A~2%" port))

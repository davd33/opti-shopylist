(in-package :db)

(defparameter *connection* nil)
(defun connect (db-name username password)
  (when (null *connection*)
    (setf *connection*
          (mito:connect-toplevel :postgres
                                 :database-name db-name
                                 :username username
                                 :password password))))

(mito:deftable shopylist-item ()
  ((timestamp :col-type :timestamp)
   (name :col-type (:varchar 128))
   (bought :col-type (:char 4))))

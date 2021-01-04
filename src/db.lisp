(in-package :db)

(defparameter all-tables '(shopping-item))

;;; --- MANAGE CONNECTION

(defvar *connection* nil)
(defun connect (db-name username password)
  (when (null *connection*)
    (setf *connection*
          (mito:connect-toplevel :postgres
                                 :database-name db-name
                                 :username username
                                 :password password))))

;;; --- CREATE/DROP TABLES

(defun create-table (table-type)
  "Creates the table of given type."
  (restart-case
      (when (not (mito.db:table-exists-p *connection*
                                         (mito.class:table-name (find-class table-type))))
        (format t "~&CREATE TABLE: ~A" table-type)
        (mapc #'mito:execute-sql (mito:table-definition table-type))
        (mito:ensure-table-exists table-type)
        t)
    (skip () nil)))

(defun create-tables ()
  (mapcar #'create-table all-tables))

(defun reset-db-tables ()
  (mapcar #'mito:recreate-table all-tables))

;;; --- DEFINE TABLES

(mito:deftable shopping-item ()
  ((product-name :col-type (:varchar 128))
   (status :col-type (:char 4))))

;;; --- QUERIES

(defun create-shopping-item (product-name
                             bought-p)
  (if (> (mito:count-dao 'shopping-item :product-name product-name)
         0)
      (set-bought-item product-name bought-p)
      (mito:insert-dao
       (make-instance 'shopping-item
                      :product-name product-name
                      :status (if bought-p
                                  "BGHT"
                                  "2BUY")))))

(defun get-all ()
  (mito:select-dao 'shopping-item
    (sxql:where
     (:not-in :status '("DEL")))
    (sxql:order-by :status (:desc :updated_at))))

(defun get-shopping-item (product-name)
  (first
   (mito:select-dao 'shopping-item
     (sxql:where (:like :product-name product-name))
     (sxql:limit 1))))

(defun set-bought-item (product-name bought-p)
  (mito:execute-sql
   (sxql:update :shopping_item
     (sxql:set= :status (if bought-p
                            "BGHT"
                            "2BUY")
                :updated_at (local-time:now))
     (sxql:where (:like :product_name product-name)))))

(defun delete-shopping-item (product-name)
  (mito:execute-sql
   (sxql:update :shopping_item
     (sxql:set= :status "DEL")
     (sxql:where (:like :product_name product-name)))))

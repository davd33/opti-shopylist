(in-package #:shopping-db-impl)

(defmethod add-shopping-item ((kind (eql :DATABASE)) (item shopping-item))
  (db:create-shopping-item (shopping-item-name item)
                           (shopping-item-bought item)))

(defmethod remove-shopping-item ((kind (eql :DATABASE)) (item shopping-item))
  (db:delete-shopping-item (shopping-item-name item)))

(defmethod set-item-bought ((kind (eql :DATABASE)) (item shopping-item) bought-p)
  (db:set-bought-item (shopping-item-name item) bought-p))

(defmethod get-all ((kind (eql :DATABASE)))
  (mapcar (lambda (db-shopping-item)
            (make-shopping-item :timestamp (slot-value db-shopping-item
                                                       'mito.dao.mixin::updated-at)
                                :name (slot-value db-shopping-item
                                                  'db:product-name)
                                :bought (string= (slot-value db-shopping-item 'db:status)
                                                 "BGHT")))
          (db:get-all)))

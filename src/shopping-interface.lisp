(in-package #:shopping-interface)

(defgeneric add-shopping-item (kind item)
  (:documentation "Add an ITEM to the shopping list"))

(defgeneric remove-shopping-item (kind item)
  (:documentation "Remove an ITEM from the shopping list"))

(defgeneric set-item-bought (kind item bought-p)
  (:documentation "Toggle bought flag of ITEM in shopping list"))

(defgeneric get-all (kind)
  (:documentation "Returns the whole list of shopping items."))

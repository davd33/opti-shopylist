(defpackage :opti-shopylist/tests/make-shopping-list-manager
  (:use :cl :rove :shopping-interface)
  (:export #:get-all
           #:add-shopping-item
           #:remove-shopping-item
           #:set-item-bought))
(in-package :opti-shopylist/tests/make-shopping-list-manager)

;;; --- SOME MOCKS

(defmethod get-all ((kind (eql :TEST)))
  "all the list")

(defmethod add-shopping-item ((kind (eql :TEST)) item)
  (declare (ignore item))
  "added to shopping list")

(defmethod remove-shopping-item ((kind (eql :TEST)) item)
  (declare (ignore item))
  "item removed")

(defmethod set-item-bought ((kind (eql :TEST)) item bought-p)
  (declare (ignore item))
  (str:concat "item updated to " (if bought-p "BOUGHT" "TO BE BOUGHT")))

;;; --- TESTS

(deftest make-shopping-list-manager
  (testing "the shopping list manager"
    (let ((manager (web-site:make-shopping-list-manager :TEST)))
      (testing "gets all the shopping list"
        (ok (funcall manager :|GET-SHOPPING-LIST|)
            "all the list"))
      (testing "adds an item"
        (ok (funcall manager :|ADD-SHOPPING-ITEM| "item")
            "added to shopping list"))
      (testing "removes an item"
        (ok (funcall manager :|REMOVE-SHOPPING-ITEM| "item")
            "item removed"))
      (testing "set the status of an item to 'bought'"
        (ok (funcall manager :|SET-BOUGHT|)
            "item updated to BOUGHT"))
      (testing "set the status of an item to 'to be bought'"
        (ok (funcall manager :|SET-NOT-BOUGHT|)
            "item updated to TO BE BOUGHT")))))

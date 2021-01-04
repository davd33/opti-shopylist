(in-package #:shopping-mem-impl)

(defvar *shopping-list* nil)

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

(defmethod add-shopping-item ((kind (eql :IN_MEMORY)) (item shopping-item))
  (setf *shopping-list*
        (adjoin item *shopping-list*
                :key (compose #'str:upcase #'shopping-item-name) :test #'string=)))

(defmethod remove-shopping-item ((kind (eql :IN_MEMORY)) (item shopping-item))
  (setf *shopping-list*
        (remove (shopping-item-name item) *shopping-list*
                :test #'string=
                :key #'shopping-item-name)))

(defmethod set-item-bought ((kind (eql :IN_MEMORY)) (item shopping-item) bought-p)
  (setf *shopping-list*
        (substitute (make-shopping-item :timestamp (get-universal-time)
                                        :name (shopping-item-name item)
                                        :bought bought-p)
                    item
                    *shopping-list*
                    :test (lambda (a b)
                            (string= (shopping-item-name a)
                                     (shopping-item-name b))))))

(defmethod get-all ((kind (eql :IN_MEMORY)))
  (setf *shopping-list*
        (stable-sort *shopping-list* #'shopping-item<)))

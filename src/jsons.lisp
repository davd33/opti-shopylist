(in-package #:jsons)

(defun get-in (json &rest fields)
  (loop for f in fields
     for res = (cdr (assoc f json)) then (cdr (assoc f res))
     finally (return res)))

(defun type-compatible-p (json class-symbol &optional array-p)
  "Return T if JSON is compatible with CLASS.
If ARRAY-P is T then checks TYPE-COMPATIBLE-P for each element of JSON.
In order to manage fields that take on lists of objects, the name of the
field must end with '-list'."
  (if array-p
      (loop
         for elt in json
         do (when (not (type-compatible-p elt class-symbol))
              (return nil))
         finally (return t))
      (labels ((contains-slot-p (json slot-name)
                 "Return true if JSON contains a field SLOT-NAME."
                 ;; A JSON could be:
                 ;;  - a number: 12
                 ;;  - a string: "hello world"
                 ;;  - an object: ((:FIELD1 . 12) (:FIELD2 . 21))
                 ;;  - a list of objects: (((:FIELD1 . 12) (:FIELD2 . 21))
                 ;;                        ((:FIELD1 . 12) (:FIELD2 . 21)))
                 ;;  - a list of values: (12 21)
                 ;;  - a mix: (12 ((:FIELD1 . 12) (:FIELD2 . 21)) 21)
                 (position (string slot-name) json
                           :key (alexandria:compose #'string #'first)
                           :test #'string=)))
        (loop
           for (slot type value) in
             (mapcar #'(lambda (elt) (let ((name (closer-mop:slot-definition-name elt))
                                           (type (closer-mop:slot-definition-type elt)))
                                       (list name type (handler-case
                                                           (get-in json (intern (string name) "KEYWORD"))
                                                         (type-error ()
                                                           (return-from type-compatible-p nil))))))
                     (mop:class-slots class-symbol))
           do
             (when (not (and (or (str:ends-with? (str:upcase "-o")
                                                 (str:upcase (string slot)))
                                 (contains-slot-p json slot))
                             (if (and (not (null value)) (listp value))
                                 (type-compatible-p value type
                                                    (str:ends-with? (str:upcase "-list")
                                                                    (str:upcase (string slot))))
                                 t)))
               (return nil))
           finally (return t)))))

(defun add-value (value json &rest fields)
  (let ((merged-alist (apply #'alists:merge-acons value json fields)))
    (loop for k in (reduce #'(lambda (a c)
                               (adjoin (car c) a))
                           (reverse merged-alist)
                           :initial-value (list))
       collect (assoc k merged-alist))))

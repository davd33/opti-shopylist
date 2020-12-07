(in-package #:mop)

(defun find-class-slots (class-symbol)
  "Return the list of slots for CLASS-SYMBOL."
  (funcall (alexandria:compose #'(lambda (slots)
                                   (mapcar #'(lambda (elt)
                                               (closer-mop:slot-definition-name elt))
                                           slots))
                               #'closer-mop:class-slots
                               #'find-class)
           class-symbol))

(defun class-slots (class-symbol)
  "Return the list of slot definitions for CLASS-SYMBOL."
  (funcall (alexandria:compose #'(lambda (class)
                                     (handler-case (closer-mop:class-slots class)
                                       (sb-int:simple-reference-error (e)
                                         (format t "~&WARNING: Class not finalized. ~A" e)
                                         (closer-mop:finalize-inheritance class)
                                         (closer-mop:class-slots class))))
                               #'find-class)
           class-symbol))

(defmacro defprintobj (class-symbol)
  "Give me a class symbol and I will defmethod a print-object that format every bound field!"
  (let* ((slot-names-fn (alexandria:compose #'(lambda (slots)
                                                (mapcar #'(lambda (elt)
                                                            (closer-mop:slot-definition-name elt))
                                                        slots))
                                            #'closer-mop:class-slots
                                            #'find-class))
         (fields (funcall slot-names-fn class-symbol)))
    `(defmethod print-object ((object ,class-symbol) stream)
       (print-unreadable-object (object stream :type t)
         (loop for field in ',fields
            do (alexandria:when-let (f-value (handler-case (slot-value object field)
                                               (unbound-slot (e) nil)))
                 (format stream "~&~A = ~A" field f-value)))))))

(defmacro make-mapper (from-class to-class &body body)
  "Creates a function that, given an input object of type 'from-class',
will make a new instance of type 'to-class' setting all name-identique
slots' values equal.

'from-class' and 'to-class' must be symbols.

In the body, you can setf slots for which the name wasn't equivalent in input and
output classes.
Available functions in the body:
  WITH-RENAMED-SLOT (old-name new-name)
    Copies the value of the 'old-name' slot in the from object to the 'new-name' slot of
    the destination object.
  WITH-MAPPED-SLOT (old-name new-name mapper)
    Use MAPPER to map value in OLD-NAME to NEW-NAME in the destination object.
  WITH-COMPUTED-SLOT (slot-name value)
    Sets value of the 'slot-name' slot of the destination object to 'value'."
  (let* ((slot-names-fn (compose #'(lambda (slots)
                                     (mapcar #'(lambda (elt)
                                                 (closer-mop:slot-definition-name elt))
                                             slots))
                                 #'(lambda (class)
                                     (handler-case (closer-mop:class-slots class)
                                       (sb-int:simple-reference-error (e)
                                         (format t "~&WARNING: Class not finalized. ~A" e)
                                         (closer-mop:finalize-inheritance class)
                                         (closer-mop:class-slots class))))
                                 #'find-class))
         (from-slots (funcall slot-names-fn from-class))
         (to-slots (funcall slot-names-fn to-class)))
    `(lambda (from-obj)
       (let ((to-obj (make-instance ',to-class)))
         (loop for to-s in ',to-slots
            do (when-let* ((from-s (find to-s ',from-slots
                                         :test #'(lambda (elt1 elt2)
                                                   (string= (string elt1)
                                                            (string elt2)))))
                           (from-value (handler-case (slot-value from-obj from-s)
                                         (unbound-slot () nil))))
                 (setf (slot-value to-obj to-s) from-value)))
         (labels ((with-renamed-slot (old-name new-name)
                    (setf (slot-value to-obj new-name)
                          (slot-value from-obj old-name)))
                  (with-mapped-slot (old-name new-name mapper)
                    (setf (slot-value to-obj new-name)
                          (funcall mapper (slot-value from-obj old-name))))
                  (with-computed-slot (slot-name value)
                    (setf (slot-value to-obj slot-name)
                          value)))
           ,@body)
         to-obj))))

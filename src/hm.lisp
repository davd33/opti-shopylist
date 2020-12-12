(in-package #:hm)

;; Hash table utils

(defun print-elt (k v &key (stream t))
  "Prints one element with K key and V value."
  (format stream "~&~a -> ~a" k v))

(defun print-all (ht)
  "Use PRINT-ELT and MAPHASH to print all the k/v pairs of HT."
  (maphash #'print-elt ht))

(defmacro put (hash-table key value)
  "Adds a key/value pair in hash-table."
  `(setf (gethash ,key ,hash-table) ,value))

(defmacro get (hash-table key)
  "Get value in hash-table for key."
  `(gethash ,key ,hash-table))

(defmethod one ((ht hash-table))
  "Get first key/value pair of HASHTABLE using MAPHASH.
The value is returned first and then the key (use multiple-value-bind to bind it)."
  (block htmap
    (maphash #'(lambda (k v)
                 (return-from htmap (values v k)))
             ht)))

(defun reduce (fn hashmap initial-value)
  "Do a reduce on a hashmap providing key and value in FN.

FN should take the following arguments:
 - ACCUMULATOR: the built result
 - KEY: the current key
 - VALUE: the current value

The initial value must be provided."
  (let ((result initial-value))
    (maphash #'(lambda (k v)
                 (setf result (funcall fn result k v)))
             hashmap)
    result))

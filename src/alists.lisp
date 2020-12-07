(in-package #:alists)

(defparameter A '((:a (:b . 11) (:c . "hello"))
                  (:d . 32))
  "Example json to test the current package's functions.")

(defun merge-acons (value alist &rest tree-path)
  "Merge in a tree-structured alist 'value' following the 'tree-path'."
  (labels ((contains-field? (alist f)
             (member f alist :key #'first)))
    (let* ((ff (first tree-path))
           (needs-to-be-merged (contains-field? alist ff)))
      (if (and needs-to-be-merged
               (not (atom (jsons:get-in needs-to-be-merged ff))))
          (acons ff
                 (apply #'merge-acons value
                        (jsons:get-in needs-to-be-merged ff)
                        (rest tree-path))
                 alist)
          (apply #'deep-acons value
                 alist
                 tree-path)))))

(defun deep-acons (value alist &rest fields)
  "Like acons, but actually adding acons of acons forming a tree structure."
  (let ((rfields (reverse fields))
        (len-f (length fields)))
    (loop for f in rfields
       for end from 1 to len-f
       for res = (if (< end len-f)
                     (acons f value nil)
                     value) then (if (< end len-f)
                                     (acons f res nil)
                                     res)
       finally (return (acons f res alist)))))

(defun aconses (alist &rest fields)
  "Adds several key/value fields to alists and return the new version."
  (loop for (k v) in fields
     for res = (if (listp k)
                   (apply #'deep-acons v alist k)
                   (acons k v alist)) then (if (listp k)
                                               (apply #'deep-acons v res k)
                                               (acons k v res))
     finally (return res)))

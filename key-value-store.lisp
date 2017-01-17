;;; key-value-store.lisp
;;; 17-Jan-2017 Shannon Spires <ssos@bearlanding.com>

;; Copyright (c) 2017, Shannon Spires
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.

;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in
;;     the documentation and/or other materials provided with the
;;     distribution.

;;   * Neither Shannon Spires nor the names of its contributors of the
;;     software may be used to endorse or promote products derived from
;;     this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Defines a generic API for key/value stores.

(in-package :kvs)

(defgeneric relate (store key value &key test)
  (:documentation "Create a key/value association in store.
    If key already exists in store, this adds a new association
    but doesn't remove the old association(s) for that key.
    Returns [possibly modified] store. Second value returned is true
    if store was actually modified.
    In some stores (like alists) you can specify a test for matching the given key.
    In other stores (like hashtables), test is ignored because it's a property of the data structure itself."))

(defgeneric relate-unique (store key value &key test)
  (:documentation "Create a key/value association in store.
    If key already exists in store, this replaces that association.
    Returns [always-modified] store."))

(defgeneric tally (store key amount &key test)
  (:documentation
   "Increments the value in the key/value pair in the given data-structure by the indicated amount.
   (Implies only one value is associated with key in store.)
   In some stores (like alists) you can specify a test for matching the given key.
   In other stores (like hashtables), test is ignored because it's a property of the data structure itself.
   Returns 2 values: New store and new total.")
  (:method :around (store key amount &key test)
    (declare (ignore store key test))
    (check-type amount number)))

(defgeneric lookup-key (store key &key test default)
  (:documentation "Returns value(s) associated with key in store, if any, or
   default if none. If multiple values are associated with key, they
   are returned as a collection of some kind, not as multiple values.
   In some stores (like alists) you can specify a test for matching the given key.
   In other stores (like hashtables), test is ignored because it's a property of the data structure itself."))

(defgeneric remove-key (store key &key test)
  (:documentation "Removes key and any value(s) associated
    with it from store. Returns [possibly modified] store. Second value returned is true
    if store was actually modified.
    In some stores (like alists) you can specify a test for matching the given key.
    In other stores (like hashtables), test is ignored because it's a property of the data structure itself."))

(defgeneric clear-store (store)
  (:documentation "Removes all associations from store and returns
    empty store."))

(defmethod relate ((store hash-table) key value &key (test #'equal))
  ; test here is used only to verify whether value is a member of existing set of values.
  ; key-matching test is a property of the store
  (let ((oldvalue (gethash key store)))
    (cond ((listp oldvalue) ; lists are the only collections for multiple values at the moment
           (if (member value oldvalue :test test)
               (values store nil)
               (progn
                 (setf (gethash key store) (cons value oldvalue))
                 (values store t))))
          (t
           (if (funcall test value oldvalue)
               (values store nil)
               (progn
                 (setf (gethash key store) (list value oldvalue))
                 (values store t)))))))

(defmethod relate ((store list) key value &key (test #'equal))
  (if (assoc key store :test test)
      (values store nil)
      (values (acons key value store) t)))

(defmethod relate-unique ((store hash-table) key value &key test)
  (declare (ignore test)) ; test is solely a property of the store
  (setf (gethash key store) value)
  store)

(defmethod relate-unique ((store list) key value &key (test #'equal))
  (let ((pair (assoc key store :test test)))
    (cond (pair
           (setf (cdr pair) value)
           store)
          (t (acons key value store)))))

(defmethod tally ((store hash-table) key amount &key test)
  (declare (ignore test)) ; test is solely a property of the store
  (values store
          (if (gethash key store)
              (incf (gethash key store) amount)
              (setf (gethash key store) amount))))

(defmethod tally ((store list) key amount &key (test #'equal))
  "Increments the key . value pair in alist indicated by key, by the indicated amount.
  If such a pair doesn't exist, create it."
  (let ((pair (assoc key store :test test)))
    (cond (pair (values store (incf (cdr pair) amount)))
          (t (values (acons key amount store)
                     amount)))))

(defmethod lookup-key ((store hash-table) key &key test (default nil))
  (declare (ignore test)) ; test is solely a property of the store
  (gethash key store default))

(defmethod lookup-key ((store list) key &key (test #'equal) (default nil))
  (or (cdr (assoc key store :test test)) default))

(defmethod remove-key ((store hash-table) key &key test)
  (declare (ignore test)) ; test is solely a property of the store
  (values store (remhash key store)))
  
(defmethod remove-key ((store list) key &key (test #'equal))
  (let* ((was-present? nil)
         (newstore (remove-if (lambda (pair)
                                (when (funcall test key (car pair))
                                  (setf was-present? t)))
                              store)))
    (if was-present? ; remove-if is not guaranteed to return an eq list if it did nothing
        (values newstore was-present?)
        (values store was-present?))))

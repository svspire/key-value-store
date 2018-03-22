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

;;; By using the functions herein (with setf in front...see below),
;;;   it's easy to change a key/value store from one mechanism to another with only one line of code.

;;; Purpose of this library is to create a key/value store mechanism with consistent
;;;   call and return semantics regardless of how the underlying store is implemented, which
;;;   Common Lisp does not natively provide.
;;;   This allows one to override the default methods herein for common key/value stores
;;;   (alists and hashtables) to also work
;;;   on many other types of stores, and the calling semantics are consistent such that
;;;   widespread changes are not needed in code that changes from one underlying storage
;;;   mechanism to another.

;;; Property lists are not supported herein, because it's not possible to distinguish a plist
;;;   from an alist with method specialization, and I prefer alists. Any storage mechanism of
;;;   type list is assumed to be an alist.

;;; The other reason I wrote this is that setf semantics are often wrong for my purposes.
;;;   In many cases, I don't care about the value I just set; I'd much rather have the
;;;   store itself returned as the primary value because this makes composition easier.
;;;   Therefore, the modifier functions herein (#'relate, #'relate-unique, #clear-store,
;;;   #'remove-key, and #'tally) always
;;;   return the store itself as their primary returned value. In the case of #'relate, a second
;;;   boolean value is returned that is true if the value in the store was actually modified.
;;;   In the case of #'tally--which is more special-purpose--the second value is the new tally
;;;   amount.
;;;   The inquiry method #'lookup-key works much like #'gethash in common lisp; it returns the
;;;   value matching a given key. We differ from #'gethash in that the first
;;;   argument here is the store itself and the second is the key. Again, this makes composition easier
;;;   and it also makes methods easier to specialize, since you're much more likely to want to
;;;   specialize on the store and not the key, and it's usually more efficient and more aesthetic to
;;;   have a defmethod's specialized argument come first. One additional difference
;;;   in #'lookup-key is that it always accepts a default argument, like #'gethash but
;;;   unlike #'assoc.

;;;   Note 1: It's always a good idea to call the modifier functions herein with a setf in front, e.g.
;;;     (setf my-store (relate my-store key value)). If my-store is an alist, this is mandatory,
;;;     because we don't guarantee that we destructively modify alists. We *can* but not always.
;;;     If my-store is a hashtable, prepending setf is optional,
;;;     because hashtables are always destructively modified. Allowing consistent use of prepended
;;;     setf was part of what I meant above by making composition easier; without this semantics,
;;;     changing a key/value store from an alist to a hashtable would have required code changes wherever
;;;    the store was modified.

;;;   Note 2: For alist stores, we try not to cons up a new list in #'relate when we can avoid it. IOW, if the
;;;    (key . value) pair in question is already present when #'relate is called, we don't cons up a new list.
;;;    But in #'relate-unique, we don't check for an existing value, so we sometimes cons and we sometimes
;;;    destructively modify the list.


(in-package :kvs)

(defgeneric relate (store key value &key test)
  (:documentation "Create a key/value association in store.
    If key already exists in store, this adds a new association
    but doesn't remove the old association(s) for that key.
    Returns [possibly modified] store. Second value returned is true
    if store was actually modified.
    In some stores (like alists) you can specify a test for matching the given key.
    In other stores (like hashtables), test is ignored because it's a property of the data structure itself.
    IMPORTANT NOTE: This conses a new list if necessary if the store is an alist. In that case,
    it's important to do (setf alist (relate alist key value)), because it doesn't destructively
    modify alists."))

(defgeneric relate-unique (store key value &key test)
  (:documentation "Create a key/value association in store.
    If key already exists in store, this replaces that association.
    Returns [always-modified] store. No second value."))

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
  ;; If the (key . value) pair in question is already present, we won't cons up a new list."
  ;; Caveats: 1. Using same test for both key and value might not always be what you want, but that's how it works here.
  ;;          2. Using this function for alists can be expensive because an exhaustive search is sometimes necessary.
  ;;             Use relate-unique on alists for better efficiency, and when you don't want duplicate keys.
  (if
   (find-if (lambda (pair) ; can't use assoc because we have to check both key and value.
              (and (funcall test key (car pair))
                   (funcall test value (cdr pair))))
            store)
   (values store nil)
   (values (acons key value store) t)))

(defmethod relate-unique ((store hash-table) key value &key test)
  (declare (ignore test)) ; test is solely a property of the store
  (setf (gethash key store) value)
  store)

(defmethod relate-unique ((store list) key value &key (test #'equal))
  "May destructively modify store. But you should still use (setf store (relate-unique store key value))."
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

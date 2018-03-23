;;; kvs-tests.lisp
;;; 22-Mar-2018 SVS
;;; Tests for key-value-store system

(in-package :kvs-test)

(defparameter *test-pairs*
  '((fee . bee)
    (foo . boo)
    (faz . baz)
    (far bar car dar)
    ))

(defun inithash (&optional (initlist *test-pairs*))
  "Copy test-pairs into hashtable and try retrieving from there.
   This also implicitly tests relate-unique for hashtables."
  (let ((table (make-store :hashtable :test #'equal)))
    (dolist (pair initlist)
      (setf table (relate-unique table (car pair) (cdr pair))))
    table))

(define-test alist-pair-retrieval
             "Test lookup-key and remove-key for an alist"
  (let ((pairs (copy-list *test-pairs*)))
    (dolist (pair pairs)
      (assert-equal (cdr pair) (lookup-key pairs (car pair)) (car pair)))
    (setf pairs (remove-key pairs 'foo))
    (assert-equal '((foo . boo)) (set-difference *test-pairs* pairs))))

; (run-tests '(alist-pair-retrieval))

; Copy *test-pairs* into a hashtable and test from there
(define-test hashtable-pair-retrieval
             "Copy *test-pairs* into a hashtable and test again"
  (let ((pairs (inithash)))
    (dolist (pair *test-pairs*)
      (assert-equal (cdr pair) (lookup-key pairs (car pair)) (car pair)))
    (setf pairs (remove-key pairs 'foo))
    (let ((readout (loop for key being each hash-key of pairs using (hash-value val)
                     collect (cons key val))))
      (assert-equalp '((foo . boo)) (set-difference *test-pairs* readout :test #'equalp) readout))))

; (run-tests '(hashtable-pair-retrieval))

(defun test-relate-unique (table)
  ; non-present key
  (multiple-value-bind (value present-p) (lookup-key table 'foo)
    (assert-nil value)
    (assert-nil present-p))
  ; present key, non-nil value
  (kvs::relate-unique! table 'foo 'baz)
  (multiple-value-bind (value present-p) (lookup-key table 'foo)
    (assert-eq 'baz value)
    (assert-true present-p))
  ; present key but nil value
  (kvs::relate-unique! table 'goo nil)
  (multiple-value-bind (value present-p) (lookup-key table 'goo)
    (assert-eq nil value)
    (assert-true present-p))
  ; change to non-nil value
  (kvs::relate-unique! table 'goo 'gaz)
  (multiple-value-bind (value present-p) (lookup-key table 'goo)
    (assert-eq 'gaz value)
    (assert-true present-p))
  )

(defun test-relate (table)
  ; non-present key
  (multiple-value-bind (value present-p) (lookup-key table 'foo)
    (assert-nil value)
    (assert-nil present-p))
  ; present key, non-nil value
  (kvs::relate! table 'foo 'baz)
  (multiple-value-bind (value present-p) (lookup-key table 'foo)
    (assert-eq 'baz value)
    (assert-true present-p))
  ; present key but nil value
  (kvs::relate! table 'goo nil)
  (multiple-value-bind (value present-p) (lookup-key table 'goo)
    (assert-eq nil value)
    (assert-true present-p))
  ; add non-nil value
  (kvs::relate! table 'goo 'gaz)
  (multiple-value-bind (value present-p) (lookup-key table 'goo)
    (assert-equal '(gaz) value)
    (assert-true present-p))
  ; add one more non-nil value
  (kvs::relate! table 'goo 'gar)
  (multiple-value-bind (value present-p) (lookup-key table 'goo)
    (assert-true (unordered-equal '(gaz gar) value))
    (assert-true present-p))
  )

(define-test alist-relate-unique
             "Test relate-unique for an alist"
             (let ((table (make-store :alist)))
               (test-relate-unique table)))

; (run-tests '(alist-relate-unique))

(define-test hashtable-relate-unique
             "Test relate-unique for a hashtable"
             (let ((table (make-store :hashtable :test #'equal)))
               (test-relate-unique table)))

; (run-tests '(hashtable-relate-unique))

(define-test alist-relate
             "Test relate for an alist"
  (let ((table (make-store :alist)))
    (test-relate table)))

; (run-tests '(alist-relate))

(define-test hashtable-relate
             "Test relate for a hashtable"
             (let ((table (make-hash-table)))
               (test-relate table)))

; (run-tests '(hashtable-relate))



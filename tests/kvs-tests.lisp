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

;(run-tests '(alist-pair-retrieval))

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

(define-test alist-relate-unique
             )

(define-test hashtable-relate-unique
             )

(define-test alist-relate
             )

(define-test hashtable-relate
             )
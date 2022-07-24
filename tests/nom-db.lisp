(defpackage cl-nom/tests/nom-db
  (:use :cl
        :cl-nom
        :rove))

(in-package :cl-nom/tests/nom-db)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-nom)' in your Lisp.

(defun set-db-path ()
  (let ((db-path (asdf:system-relative-pathname "cl-nom" "current-dbs/")))
    (setf z/nom::*db-root* (namestring db-path))))

(deftest db-keys-test
  (let ((known-address
	 #(0 41 33 199 50 187 228 92 2 116 180 106 250 91 130 93 63 83 22 177)))
    (set-db-path)
    (ok (equalp #(85 3 ;; frontier / account prefix
		  0 41 33 199 50 187 228 92 2 116
		  180 106 250 91 130 93 63 83 22 177
		  2 ;; height prefix
		  0 0 0 0 0 0 0 2) ;; height
		(z/nom::account-block-for-height-key known-address 2)))))

(deftest account-block-extraction-test
  (set-db-path)
  (let* ((account-blocks-and-momentum
	  (z/nom::prefetch-momentum (zi/nom-db::momentum-by-height 2)))
	 (known-block (first (first account-blocks-and-momentum))))
    (ok (= 30800000000
	   (z/nom::account-block-amount known-block)))))

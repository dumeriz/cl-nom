(defpackage cl-nom/tests/bytes
  (:use :cl
        :cl-nom
        :rove))

(in-package :cl-nom/tests/bytes)

(deftest byte-array-to-int-test
  (testing "0x0F00 is 3840"
    (ok (= 3840 (z/bytes:byte-array-to-int #(15 0)))))
  (testing "0x000F is 15"
    (ok (= 15 (z/bytes:byte-array-to-int #(0 15)))))
  (testing "`rev` reverses byte order"
    (ok (= (z/bytes:byte-array-to-int #(1 2 3 4)) (z/bytes:byte-array-to-int #(4 3 2 1) :rev t))))
  (testing "start is always > end"
    (ok (= (z/bytes:byte-array-to-int #(1 2 3 4)) (z/bytes:byte-array-to-int #(1 2 3 4) :start 4 :end 0))))
  (testing "limited range is considered"
    (ok (= 3840 (z/bytes:byte-array-to-int #(1 2 15 0 5 6) :start 2 :end 4))))
  (testing "regression test for 308 ZNN (first account block in height 2)"
    (ok (= 30800000000 (z/bytes:byte-array-to-int #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 43 210 180 0))))))

(deftest int-to-byte-array-test
  (testing "Regression test for frontier height"
    (ok (equalp #(0 16 194 79)
		(z/bytes:int-to-byte-array 1098319 :width 4)))))

(deftest bytes-as-uint64-test
  (testing "0x0F00 is 3840"
    (ok (= 3840 (z/bytes:byte-octet-as-uint64 #(0 0 0 0 0 0 15 0)))))
  (testing "0x000F is 15"
    (ok (= 15 (z/bytes:byte-octet-as-uint64 #(0 0 0 0 0 0 0 15)))))
  (testing "back-and-forth-conversion bytes->num->bytes"
    (ok (equalp #(1 2 3 4 5 6 7 8)
		(z/bytes:uint64-as-byte-octet
		 (z/bytes:byte-octet-as-uint64 #(1 2 3 4 5 6 7 8)))))))

(deftest uint64-as-bytes-test
  (testing "3840 is 0x0f00"
    (ok (equalp #(0 0 0 0 0 0 15 0) (z/bytes:uint64-as-byte-octet 3840))))
  (testing "0x000F is 15"
    (ok (equalp #(0 0 0 0 0 0 0 15) (z/bytes:uint64-as-byte-octet 15))))
  (testing "back-and-forth-conversion num->bytes->num"
    (ok (= 18446744073709551615
	   (z/bytes:byte-octet-as-uint64
	    (z/bytes:uint64-as-byte-octet 18446744073709551615))))))

(deftest make-byte-octet-test
  (testing "regular 8-byte-sequence"
    (ok (equalp #(1 2 3 4 5 6 7 8)
		(z/bytes::make-byte-octet '(1 2 3 4 5 6 7 8 )))))
  (testing "0's prepended for shorter sequence"
    (ok (equalp #(0 0 0 0 1 2 3 4)
		(z/bytes::make-byte-octet '(0 0 0 0 1 2 3 4)))))
  (testing "First elements discarded for longer sequence"
    (ok (equalp #(0 0 0 0 1 2 3 4)
		(z/bytes::make-byte-octet '(9 10 0 0 0 0 1 2 3 4))))))


(defpackage :zenon-ndb-bytes
  (:use :cl)
  (:nicknames :z/bytes)
  (:export :bytes
	   :byte-array
	   :make-byte-octet
	   :byte-array-to-int
	   :int-to-byte-array
	   :byte-octet-as-uint64
	   :uint64-as-byte-octet))

(in-package :zenon-ndb-bytes)

(defun bytes (num &rest rest)
  (fast-io:octets-from (append `(,num) rest)))

(defun make-byte-octet (seq)
  "Transforms `seq` into a `(vector (unsigned-byte 8) 8)`.
If `seq` has fewer than 8 elements, 0-bytes are prepended.
If `seq` has more than n > 8 elements, the first n-8 elements are discarded."
  (let ((len (length seq)))
    (fast-io:octets-from
     (cond
       ((= 8 len) seq)
       ((< 8 len) (subseq seq (- len 8)))
       (t
	(nreverse
	   (map-into (list 0 0 0 0 0 0 0 0) 'identity (reverse seq))))))))
     
(defun byte-array-to-int (arr &key (start 0) (end (length arr)) (rev nil))
  "Interpret `arr` as number in big-endian interpretation.
`start` and `end` limit the range, when `rev` is `t` the byte order is reversed."
  (when (> start end) (rotatef start end))
  (reduce (lambda (x y) (logior (ash x 8) y))
	  (if rev (reverse arr) arr)
	  :start start
	  :end end))

(defun byte-array (width &key initial-bytes from-end)
  (let ((arr
	 (make-array width :element-type '(unsigned-byte 8))))
    (when initial-bytes
      (if from-end
	  (loop with count = (1- (min (length initial-bytes)
				      width))
	        for i from count downto 0
	        for j from (1- width) downto 0
	     do (setf (aref arr j) (aref initial-bytes i)))
	  (loop for i below (min width (length initial-bytes))
	     do (setf (aref arr i) (aref initial-bytes i)))))
    arr))

(defun int-to-byte-array (number &key width)
  "Converts `num` to an array of `width` bytes in big-endian notation."
  (let ((arr (make-array width :element-type '(unsigned-byte 8))))
    (loop
       for n = number then (ash n -8)
       for i from (1- width) downto 0
       for v = (mod n 256)
       do (setf (aref arr i) v))
    arr))

(defun byte-octet-as-uint64 (arr &key little-endian)
  "Converts the array of bytes to uint64. If `arr` does not have 8 elements,
use `byte-array-to-int`. By default, arr is interpreted in big-endian notation.
If `little-endian` is not `nil`, byte-order is reversed."
  (if little-endian
      (cl-intbytes:octets->uint64 arr)
      (cl-intbytes:octets->uint64 (reverse arr))))

(defun uint64-as-byte-octet (num &key little-endian)
  "Converts `num` to an array of 8 bytes in big endian notation unless `little-endian` is not `nil`."
  (let ((octets (cl-intbytes:int64->octets num)))
    (if little-endian octets (nreverse octets))))

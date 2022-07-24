(defpackage :zenon-nom-db
  (:use :cl)
  (:nicknames :z/nom)
  (:local-nicknames (:protos :cl-protobufs)
		    (:types :cl-protobufs.types)
		    (:nom :cl-protobufs.nom)
		    (:bytes :zenon-ndb-bytes))
  (:export :*db-root*
	   :frontier-height
	   :addresses-from-momentum-by-height))

(in-package :zenon-nom-db)

(defparameter *db-root* "")

(defun open-db (db-path)
  (leveldb:open db-path :if-does-not-exist :error))

(defun bytestring->array (bytestring)
  (map '(vector (unsigned-byte 8)) #'identity bytestring))

;; address in bytes
(defconstant address-byte-length 20)

;; common/db/versioned-db.go
(defconstant frontier-byte 85)

;; common/db/keys.go
(defconstant frontier-identifier-key 0)
(defconstant height-by-hash-prefix 1)
(defconstant entry-by-height-prefix 2)

;; momentum/keys.go
(defconstant account-store-prefix 3)
(defconstant account-mailbox-prefix 4)
(defconstant block-confirmation-height-prefix 5)
(defconstant account-znn-balance-prefix 8)
(defconstant account-header-by-hash-prefix 9)

;; chain/nom/account_block.go
;; BlockTypeGenesisReceive = 1 // receive
;; BlockTypeUserSend    = 2 // send
;; BlockTypeUserReceive = 3 // receive
;; BlockTypeContractSend    = 4 // send
;; BlockTypeContractReceive = 5 // receive

(defun prefixed-account-store (address)
  (let* ((len (length address))
	 (arr (adjust-array (reverse address) (1+ len))))
    (setf (aref arr len) account-store-prefix)
    (nreverse arr)))

(defun nom-db ()
  (concatenate 'string *db-root* "/nom"))

(defun address->bytes (string-address &key width from-end)
  (multiple-value-bind (bytes hrp)
      (bp-encoding:bech32-decode string-address)
    (if (and width (< (length bytes) width))
	(values (z/bytes:byte-array width
				     :initial-bytes bytes
				     :from-end from-end)
		hrp)
	(values bytes hrp))))

;;
;; Deserializing from protobuf data
;;

(defun deserialize-hash-height (data)
  (protos:deserialize-from-bytes 'types:hash-height-proto data 1))

(defun deserialize-momentum (data)
  (protos:deserialize-from-bytes 'nom:momentum-proto data 1))

(defun deserialize-account-block (data)
  ;; to get amount: common/bytes.go:BytesToBigint ab.amount
  ;; to get zts: common/types/tokenstandard.go:BytesToZtsPanic ab.TokenStandard
  (protos:deserialize-from-bytes 'nom:account-block-proto data 1))

;;
;; Key / DB Prefix construction
;;

(defun stable-account-db-key (address-string)
  "This is the account-store-prefix used to retrieve an account db,
stored under the frontier prefix in the chain"
  (let ((arr (address->bytes address-string :width 22 :from-end t)))
    (setf (aref arr 0) frontier-byte
	  (aref arr 1) account-store-prefix)
    arr))

(defun momentum-for-height-key (height)
  "Constructs the key that retrieves the momentum for `height`."
  (let ((arr (z/bytes::int-to-byte-array height :width 10)))
    (setf (aref arr 0) frontier-byte
	  (aref arr 1) entry-by-height-prefix)
    arr))

(defun account-block-for-height-key (address height)
  "Constructs the key that retrieves the account block for momentum at `height` for `address`."
  ;; That key is constructed as a sequence of the following elements:
  ;; frontier-byte, account-store-prefix, address, entry-by-height-prefix, height,
  ;; where the uint64 height is encoded as a big-endian byte-octet.
  ;; see GetFrontierMomentumStore (chain/momentum_pool.go:161)
  ;; and GetAccountStore (chain/momentum/ledger_store.go:35)
  ;; and GetEntryByHeight (db/store.go:62)
  (let* ((height-prefix-pos (+ 2 address-byte-length))
	 (len (+ height-prefix-pos 9)) ; see comment above
	 (arr (z/bytes::int-to-byte-array height :width len)))
    (setf (aref arr 0) frontier-byte
	  (aref arr 1) account-store-prefix
	  (aref arr height-prefix-pos) entry-by-height-prefix)
    (loop
       for i from 0 below address-byte-length
       for j from 2 below height-prefix-pos
	 do (setf (aref arr j) (aref address i)))
    arr))

;;
;; Leveldb access (nom-db)
;;

(defun db/get (what)
  (leveldb:with-open-db (db (nom-db))
    (leveldb:get db what)))

(defun db/get-frontier-identifier ()
  "Fetch the encoded frontier-identifier from the database."
  (db/get 
   (bytes:bytes frontier-byte frontier-identifier-key)))

(defun db/get-entry-by-height (height)
  "Fetch the momentum at `height` from the database."
  (db/get
   (momentum-for-height-key height)))

;; chain/momentum/account-block.go:18
(defun db/get-account-block (account-header)
  "Fetch the account block for `account-header` header from the database."
  (let ((address
	 (types:address-proto.address
	  (types:account-header-proto.address account-header)))
	(height
	 (types:hash-height-proto.height
	  (types:account-header-proto.hash-height account-header))))
    (db/get
     (account-block-for-height-key address height))))

;;
;; API
;;

(defun momentum-hash-bytes (momentum)
  "Extract the hash from `momentum`."
  (types:hash-proto.hash
   (nom:momentum-proto.hash momentum)))

(defun frontier-identifier ()
  "Get the frontier identifier."
  (deserialize-hash-height (db/get-frontier-identifier)))

(defun frontier-height ()
  "Get the height of the frontier momentum."
  (types:hash-height-proto.height (frontier-identifier)))

(defun frontier-hash-bytes ()
  "Get the hash of the frontier momentum."
  (types:hash-proto.hash
   (types:hash-height-proto.hash (frontier-identifier))))

(defun frontier-momentum ()
  "Get the frontier momentum."
  (deserialize-momentum (db/get-entry-by-height (frontier-height))))

(defun momentum-by-height (height)
  "Get the momentum at `height`."
  (deserialize-momentum (db/get-entry-by-height height)))

(defun account-block-amount (parsed-account-block)
  "Decode the byte-array in the `amount` key of `parsed-account-block` to number."
  (bytes:byte-array-to-int (nom:account-block-proto.amount parsed-account-block)))

(defun account-block-address-bytes (parsed-account-block)
  "Extracts the `address` field byte array from `parsed-account-block.`"
  (types:address-proto.address
   (nom:account-block-proto.address parsed-account-block)))

(defun address-string (address-bytes)
  "Get the readable representation for `address-bytes`."
  (bp-encoding:bech32-encode "z" address-bytes))
  
;; chain/momentum/momentum.go:64
(defun prefetch-momentum (momentum)
  "Deserialize the account blocks contained in the deserialized `momentum`.
Returns a cons with the list of blocks in the car and the momentum in th cdr."
  (let ((content (nom:momentum-proto.content momentum)))
    (loop
       for c in content
       for proto = (db/get-account-block c)
       collecting (deserialize-account-block proto) into account-blocks
       finally (return (cons account-blocks momentum)))))

(defun prefetch-momentum-by-height (height)
  (prefetch-momentum (momentum-by-height height)))

(defun addresses-from-momentum-by-height (height)
  "Get the unique `address` fields from all account blocks for momentum at `height`."
  (let ((address-blocks (car (prefetch-momentum-by-height height))))
    (remove-duplicates
     (map 'list (lambda (ab)
		  (address-string (account-block-address-bytes ab)))
	  address-blocks)
     :test #'string=)))

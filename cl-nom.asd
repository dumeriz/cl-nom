(defsystem "cl-nom"
  :version "0.1.0"
  :author "Dumeril"
  :license "MIT"
  :defsystem-depends-on (:cl-protobufs.asdf)
  :depends-on ("alexandria"
	       "str"
	       "cl-protobufs"
	       "leveldb"
	       "fast-io"
	       "cl-intbytes"
	       "ironclad")

  :components ((:module "src"
		:serial t
                :components
                ((:protobuf-source-file "types"
                  :proto-pathname "../protos/types.proto")
  		 (:protobuf-source-file "nom"
 	          :proto-pathname "../protos/nom.proto")
 		 (:protobuf-source-file "election_data"
		  :proto-pathname "../protos/election_data.proto")
		 (:protobuf-source-file "point"
		  :proto-pathname "../protos/point.proto")
		 (:file "bp/hash")     ;; borrowed from bitcoin-protocol
		 (:file "bp/encoding") ;; borrowed from bitcoin-protocol
		 (:file "bytes")
		 (:file "nom-db"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-nom/tests"))))

(defsystem "cl-nom/tests"
  :author "Dumeril"
  :license "MIT"
  :depends-on ("cl-nom"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "bytes")
		 (:file "nom-db"))))
  :description "Test system for cl-nom"
  :perform (test-op (op c) (symbol-call :rove :run c)))

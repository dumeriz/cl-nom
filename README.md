# CL-NOM

This is a utility library to work with the data in the leveldb databases used by znnd (alphanet).

In addition to the libs mentioned in cl-nom.asd, it uses files from the bitcoin protocol library for CL: https://github.com/rodentrabies/bp/.

When using this lib, make a local copy of the leveldb files to not corrupt your node. I'm using a copy in a current-dbs folder in this repository, which is also gitignore'd and referenced in the tests.

## Installation
To work with this library we have to generate the lisp stubs from the protobuf files that come with go-zenon. Versions from June 22 are included in the protos folder.
The stubs should then automatically be generated upon system load, when cl-protobufs is installed as follows:

1. If on Linux, get, make and install protobuf, as the apt-available dev versions are not installing all required headers.
   Follow the build instructions here: https://github.com/protocolbuffers/protobuf/blob/main/src/README.md. (It is possible
   that the current version is incompatible to the protobuf-files from cl-protobufs?)

   On macOS, it should be sufficient to `brew install protobuf`.
2. Setup sbcl and quicklisp:
   - `sudo apt install sbcl` or `brew install sbcl`
   - Install quicklisp as described here: https://www.quicklisp.org/beta/
2. Get cl-protobuf, build and register the protobuf-lisp plugin:
   ```bash
   git clone https://github.com/qitab/cl-protobufs.git
   cd cl-protobufs/protoc && make -j
   export PATH=$PATH:$PWD
   ```
   See the installation instructions on the cl-protobufs-git for details.
   Basically, we now want to have `protoc` and `protoc-gen-cl-pb` on our `$PATH`.
3. Add cl-protobuf to our lisp system load path:
   ```bash
   ln -s /path/to/cl-protobufs ~/quicklisp/local-projects
   ```
4. Add this system to our lisp system load path:
   ```bash
   ln -s $PWD ~/quicklisp/local-projects
   ```

We should now be able to quickload this system into a running sbcl image.

## Author

* Dumeril (zdumeril@gmail.com)

## Copyright

Copyright (c) 2022 Dumeril (zdumeril@gmail.com)

## License

Licensed under the MIT License.

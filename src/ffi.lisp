(in-package #:cl-lz4-rs)

(defun get-native-library-path ()
  "Get the full path to the native library, ensuring it exists."
  (let ((lib-path (cl-lz4-rs/build:get-library-path)))
    (unless (probe-file lib-path)
      (error "Native library not found at ~A. Try running (cl-lz4-rs/build:build-native-library)" lib-path))
    lib-path))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((system-dir (asdf:system-source-directory :cl-lz4-rs)))
    (pushnew (directory-namestring system-dir)
             cffi:*foreign-library-directories*
             :test #'equal)))

(cffi:define-foreign-library liblz4-wrapper
  (:unix (:or (:absolute (get-native-library-path))
              "liblz4_wrapper.so"))
  (:windows (:or (:absolute (get-native-library-path))
                 "lz4_wrapper.dll"))
  (:darwin (:or (:absolute (get-native-library-path))
                "liblz4_wrapper.dylib"))
  (t (:default "lz4_wrapper")))

(handler-case
    (cffi:use-foreign-library liblz4-wrapper)
  (cffi:load-foreign-library-error (e)
    (format t "~& Failed to load native library: ~A~%" e)
    (unless (cl-lz4-rs/build:auto-build-disabled-p)
      (format t "~&Attempting to build native library automatically...~%")
      (cl-lz4-rs/build:build-native-library :verbose t)
      (cffi:use-foreign-library liblz4-wrapper))))

(cffi:defcfun ("lz4_compress_bound" lz4-compress-bound) :size
  "Calculates the maximum compressed size for a given input size."
  (input-size :size))

(cffi:defcfun ("lz4_compress_into" lz4-compress-into) :int
  "Compresses data from src into dest. Returns bytes written, or negative on error."
  (src :pointer)
  (src-len :size)
  (dest :pointer)
  (dest-len :size))

(cffi:defcfun ("lz4_decompress_into" lz4-decompress-into) :int
  "Decompresses data from src into dest. Returns bytes written, or negative on error."
  (src :pointer)
  (src-len :size)
  (dest :pointer)
  (dest-len :size))

(deftype ub8-vector ()
  "Type definition for simple unsigned byte vectors."
  '(simple-array (unsigned-byte 8) (*)))

(defun compress (input-vector)
  "Compresses a simple byte vector using LZ4.
  Returns a new, correctly-sized byte vector with the compressed data.
  
  ARGUMENTS:
  INPUT-VECTOR - A simple-array of (unsigned-byte 8)
  
  RETURNS:
  A new simple-array of (unsigned-byte 8) containing the compressed data.
  
  ERRORS:
  Signals an error if compression fails or if input is not the right type."
  (check-type input-vector ub8-vector)
  
  (when (zerop (length input-vector))
    (return-from compress (make-array 0 :element-type '(unsigned-byte 8))))
  
  (let* ((input-size (length input-vector))
         (max-output-size (lz4-compress-bound input-size))
         (output-vector (make-array max-output-size :element-type '(unsigned-byte 8))))
    
    (when (> input-size most-positive-fixnum)
      (error "Input vector too large: ~D bytes (maximum ~D)" input-size most-positive-fixnum))
    
    (cffi:with-pointer-to-vector-data (p-in input-vector)
      (cffi:with-pointer-to-vector-data (p-out output-vector)
        (let ((bytes-written (lz4-compress-into p-in input-size p-out max-output-size)))
          (cond
            ((< bytes-written 0)
             (error "LZ4 compression failed with error code ~D. The output buffer might be too small or the data corrupt." 
                    bytes-written))
            ((= bytes-written 0)
             (error "LZ4 compression returned 0 bytes - this should not happen with valid input."))
            (t
             (subseq output-vector 0 bytes-written))))))))

(defun decompress (input-vector uncompressed-size)
  "Decompresses a simple byte vector using LZ4.
  
  ARGUMENTS:
  INPUT-VECTOR      - A simple-array of (unsigned-byte 8) containing compressed data
  UNCOMPRESSED-SIZE - The exact size of the original uncompressed data (integer)
  
  RETURNS:
  A new simple-array of (unsigned-byte 8) with the decompressed data.
  
  ERRORS:
  Signals an error if decompression fails, input types are wrong, or sizes don't match.
  
  NOTE:
  You must know the exact uncompressed size beforehand. LZ4 does not store this
  information in the compressed data itself."
  (check-type input-vector ub8-vector)
  (check-type uncompressed-size (integer 0 *))
  
  (when (zerop uncompressed-size)
    (return-from decompress (make-array 0 :element-type '(unsigned-byte 8))))
  
  (when (> uncompressed-size most-positive-fixnum)
    (error "Uncompressed size too large: ~D bytes (maximum ~D)" uncompressed-size most-positive-fixnum))
  
  (when (zerop (length input-vector))
    (error "Cannot decompress empty input vector to non-zero size ~D" uncompressed-size))
  
  (let ((output-vector (make-array uncompressed-size :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (p-in input-vector)
      (cffi:with-pointer-to-vector-data (p-out output-vector)
        (let ((bytes-written (lz4-decompress-into p-in (length input-vector) p-out uncompressed-size)))
          (cond
            ((< bytes-written 0)
             (error "LZ4 decompression failed with error code ~D. The input data may be corrupt or the uncompressed size incorrect." 
                    bytes-written))
            ((/= bytes-written uncompressed-size)
             (error "LZ4 decompression size mismatch: expected ~D bytes, got ~D bytes." 
                    uncompressed-size bytes-written))
            (t
             output-vector)))))))

(defun test-compression ()
  "Simple test to verify the library is working correctly."
  (let* ((test-data #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
         (compressed (compress test-data))
         (decompressed (decompress compressed (length test-data))))
    (format t "~&Original:      ~A~%" test-data)
    (format t "~&Compressed:   ~A (length ~D)~%" compressed (length compressed))
    (format t "~&Decompressed: ~A~%" decompressed)
    (format t "~&Compression ratio: ~,2F%~%" 
            (* 100.0 (/ (length compressed) (length test-data))))
    (if (equalp test-data decompressed)
        (format t "~& Test passed!~%")
        (format t "~& Test failed - data mismatch!~%"))
    (equalp test-data decompressed)))

(defun library-info ()
  "Display information about the loaded native library."
  (format t "~&=== CL-LZ4-RS Library Information ===~%")
  (format t "Library path: ~A~%" (get-native-library-path))
  (format t "Library loaded: ~A~%" 
          (if (cffi:foreign-library-loaded-p 'liblz4-wrapper) "Yes" "No"))
  (when (cffi:foreign-library-loaded-p 'liblz4-wrapper)
    (format t "Test compression: ")
    (if (test-compression)
        (format t "Working~%")
        (format t "Failed~%"))))

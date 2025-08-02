(defpackage #:cl-lz4-rs-tests
  (:use #:cl #:cl-lz4-rs #:fiveam)
  (:export #:run-tests))
(in-package #:cl-lz4-rs-tests)

(def-suite lz4-rs-suite
  :description "Test suite for the cl-lz4-rs library.")

(in-suite lz4-rs-suite)

(test correctness-test
  "Ensure that decompressing compressed data yields the original data."
  (let* ((original-data (make-array 10000 :element-type '(unsigned-byte 8)))
         (compressed-data nil)
         (decompressed-data nil))
    ;; Fill with some non-trivial data
    (dotimes (i (length original-data))
      (setf (aref original-data i) (mod i 256)))
    (setf compressed-data (compress original-data))
    (setf decompressed-data (decompress compressed-data (length original-data)))
    (is (equalp original-data decompressed-data)
        "Decompressed data should be identical to the original data.")))

(test edge-case-empty-vector
  "Test compressing and decompressing an empty vector."
  (let* ((original-data (make-array 0 :element-type '(unsigned-byte 8)))
         (compressed-data (compress original-data))
         (decompressed-data (decompress compressed-data 0)))
    (is (equalp original-data decompressed-data)
        "Decompressing a compressed empty vector should result in an empty vector.")
    (is (= 0 (length compressed-data))
        "Compressing an empty vector should result in an empty vector.")))

(test large-data-test
  "Test with larger data sizes"
  (let* ((size (* 512 1024)) ; 512KB - reasonable for tests
         (original-data (make-array size :element-type '(unsigned-byte 8))))
    ;; Fill with pseudo-random but deterministic data
    (dotimes (i size)
      (setf (aref original-data i) (mod (+ (* i 17) 42) 256)))
    (let* ((compressed (compress original-data))
           (decompressed (decompress compressed size)))
      (is (equalp original-data decompressed)
          "Large data should compress and decompress correctly")
      (is (< (length compressed) size)
          "Compressed data should be smaller than original"))))

(test random-data-test
  "Test with truly random data (should compress poorly but work correctly)"
  (let* ((size 4096)
         (original-data (make-array size :element-type '(unsigned-byte 8))))
    ;; Fill with random data using a seeded generator for reproducibility
    (let ((rand-state (make-random-state t)))
      (dotimes (i size)
        (setf (aref original-data i) (random 256 rand-state))))
    (let* ((compressed (compress original-data))
           (decompressed (decompress compressed size)))
      (is (equalp original-data decompressed)
          "Random data should compress and decompress correctly"))))

(test repetitive-data-test
  "Test with highly repetitive data (should compress very well)"
  (let* ((size 10000)
         (original-data (make-array size :element-type '(unsigned-byte 8) :initial-element 42)))
    (let* ((compressed (compress original-data))
           (decompressed (decompress compressed size)))
      (is (equalp original-data decompressed)
          "Repetitive data should compress and decompress correctly")
      (is (< (length compressed) (/ size 10))
          "Repetitive data should compress to less than 10% of original size"))))

(test single-byte-test
  "Test with single byte vectors"
  (let* ((original-data (make-array 1 :element-type '(unsigned-byte 8) :initial-element 123))
         (compressed (compress original-data))
         (decompressed (decompress compressed 1)))
    (is (equalp original-data decompressed)
        "Single byte should compress and decompress correctly")))

(test error-conditions
  "Test error conditions"
  ;; Test wrong uncompressed size
  (signals error
    (let ((compressed (compress #(1 2 3 4 5))))
      (decompress compressed 1000)))
  
  ;; Test type errors
  (signals type-error
    (compress "not a byte vector"))
  
  (signals type-error
    (compress #(1 2 3))) ; vector of fixnums, not unsigned-byte 8
  
  (signals type-error
    (decompress #(1 2 3 4) "not a number"))
  
  ;; Test decompressing empty vector to non-zero size
  (signals error
    (decompress (make-array 0 :element-type '(unsigned-byte 8)) 100)))

(test boundary-conditions
  "Test various boundary conditions"
  ;; Test with all possible byte values
  (let* ((original-data (make-array 256 :element-type '(unsigned-byte 8))))
    (dotimes (i 256)
      (setf (aref original-data i) i))
    (let* ((compressed (compress original-data))
           (decompressed (decompress compressed 256)))
      (is (equalp original-data decompressed)
          "All byte values should compress and decompress correctly")))
  
  ;; Test with alternating pattern
  (let* ((size 1000)
         (original-data (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size)
      (setf (aref original-data i) (if (evenp i) 0 255)))
    (let* ((compressed (compress original-data))
           (decompressed (decompress compressed size)))
      (is (equalp original-data decompressed)
          "Alternating pattern should compress and decompress correctly"))))

(test compression-efficiency
  "Test that compression actually reduces size for compressible data"
  (let* ((size 10000)
         ;; Create data with patterns that should compress well
         (original-data (make-array size :element-type '(unsigned-byte 8))))
    ;; Fill with repeating pattern
    (dotimes (i size)
      (setf (aref original-data i) (mod (floor i 10) 256)))
    (let ((compressed (compress original-data)))
      (is (< (length compressed) (* size 0.8))
          "Patterned data should compress to less than 80% of original size"))))

(test roundtrip-stress-test
  "Stress test with multiple rounds of compression/decompression"
  (let* ((original-data (make-array 1000 :element-type '(unsigned-byte 8)))
         (current-data nil))
    ;; Fill with mixed pattern
    (dotimes (i 1000)
      (setf (aref original-data i) (mod (+ i (* i i)) 256)))
    
    (setf current-data original-data)
    ;; Perform multiple compression/decompression cycles
    (dotimes (cycle 5)
      (let* ((compressed (compress current-data))
             (decompressed (decompress compressed (length current-data))))
        (is (equalp current-data decompressed)
            (format nil "Round-trip should work correctly on cycle ~D" (1+ cycle)))
        (setf current-data decompressed)))
    
    ;; Final check that we still have original data
    (is (equalp original-data current-data)
        "Data should be identical after multiple round-trips")))
(defun run-tests ()
  "Run all tests in the suite."
  (run! 'lz4-rs-suite))
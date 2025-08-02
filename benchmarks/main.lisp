(defpackage #:cl-lz4-rs-benchmarks
  (:use #:cl #:cl-lz4-rs #:trivial-benchmark)
  (:export #:run-all-benchmarks))
(in-package #:cl-lz4-rs-benchmarks)

(defun time-operation (operation iterations)
  "Time an operation over multiple iterations, return average time in seconds"
  (let ((timer (make-timer)))
    (start timer)
    (dotimes (i iterations)
      (funcall operation))
    (commit timer)
    (let ((samples (samples timer)))
      (if (> (length samples) 0)
          ;; Get the average real-time from samples
          (/ (reduce #'+ samples :key (lambda (sample) 
                                        (getf sample :real-time 0)))
             (length samples)
             internal-time-units-per-second)
          0.0))))

(defun format-throughput (mb-size time-seconds iterations)
  "Calculate and format throughput in MB/s"
  (if (and time-seconds (numberp time-seconds) (> time-seconds 0))
      (let ((avg-time-per-op (/ time-seconds iterations)))
        (values (/ mb-size avg-time-per-op) ; MB/s
                avg-time-per-op)) ; seconds per operation
      (values 0.0 0.0))) ; fallback for invalid timing

(defun simple-time-operation (operation iterations)
  "Simple timing using get-internal-real-time. Returns average time per operation."
  (let ((start-time (get-internal-real-time)))
    (dotimes (i iterations)
      (funcall operation))
    (let ((end-time (get-internal-real-time)))
      (if (plusp iterations)
          (/ (- end-time start-time) 
             internal-time-units-per-second
             iterations)
          0.0))))

(defun run-comparison-benchmarks (size-mb iterations)
  "Helper function to run the full suite of benchmarks for a given data size."
  (let* ((data-size (* size-mb 1024 1024))
         (original-data (make-array data-size :element-type '(unsigned-byte 8))))
    
    ;; Fill with semi-realistic data (mix of patterns and randomness)
    (dotimes (i data-size)
      (setf (aref original-data i) 
            (mod (+ i (* (mod i 100) 17) (mod i 7)) 256)))

    (format t "~&~%======================================================================~%")
    (format t "~&--- Running Comparison Benchmarks (~DMB data, ~D iterations) ---~%" size-mb iterations)
    (format t "======================================================================~%")
    (format t "~&Data size: ~,2F MB (~:D bytes)~%" size-mb data-size)

    (let ((compressed-data-lz4 (compress original-data))
          (compressed-data-zlib (salza2:compress-data original-data 'salza2:zlib-compressor)))
      
      (format t "~&~%--- Compression Ratios ---~%")
      (format t "  cl-lz4-rs:     ~,2F%% (~:D bytes)~%" 
              (* 100.0 (/ (length compressed-data-lz4) data-size))
              (length compressed-data-lz4))
      (format t "  Salza2 (zlib): ~,2F%% (~:D bytes)~%~%" 
              (* 100.0 (/ (length compressed-data-zlib) data-size))
              (length compressed-data-zlib))
      (format t "~&--- cl-lz4-rs Compression ---~%")
      (with-timing (iterations)
        (compress original-data))
      (let ((avg-time (simple-time-operation 
                        (lambda () (compress original-data))
                        1)))
        (format t "  Average time per op: ~,4F seconds~%" avg-time)
        (when (> avg-time 0)
          (format t "  Throughput: ~,2F MB/s~%~%" (/ size-mb avg-time))))

      ;; cl-lz4-rs decompression benchmark
      (format t "~&--- cl-lz4-rs Decompression ---~%")
      (with-timing (iterations)
        (decompress compressed-data-lz4 data-size))
      (let ((avg-time (simple-time-operation
                        (lambda () (decompress compressed-data-lz4 data-size))
                        1)))
        (format t "  Average time per op: ~,4F seconds~%" avg-time)
        (when (> avg-time 0)
          (format t "  Throughput: ~,2F MB/s~%~%" (/ size-mb avg-time))))
        
      ;; Salza2 compression benchmark
      (format t "~&--- Salza2 (zlib) Compression ---~%")
      (with-timing (iterations)
        (salza2:compress-data original-data 'salza2:zlib-compressor))
      (let ((avg-time (simple-time-operation
                        (lambda () (salza2:compress-data original-data 'salza2:zlib-compressor))
                        1)))
        (format t "  Average time per op: ~,4F seconds~%" avg-time)
        (when (> avg-time 0)
          (format t "  Throughput: ~,2F MB/s~%~%" (/ size-mb avg-time))))
        
      ;; Chipz decompression benchmark
      (format t "~&--- Chipz (zlib) Decompression ---~%")
      (with-timing (iterations)
        (chipz:decompress nil 'chipz:zlib compressed-data-zlib))
      (let ((avg-time (simple-time-operation
                        (lambda () (chipz:decompress nil 'chipz:zlib compressed-data-zlib))
                        1)))
        (format t "  Average time per op: ~,4F seconds~%" avg-time)
        (when (> avg-time 0)
          (format t "  Throughput: ~,2F MB/s~%~%" (/ size-mb avg-time))))

      ;; Performance comparison summary
      (format t "~&--- Performance Summary ---~%")
      (let* ((lz4-compress-time (simple-time-operation (lambda () (compress original-data)) 1))
             (lz4-decompress-time (simple-time-operation (lambda () (decompress compressed-data-lz4 data-size)) 1))
             (salza-compress-time (simple-time-operation (lambda () (salza2:compress-data original-data 'salza2:zlib-compressor)) 1))
             (chipz-decompress-time (simple-time-operation (lambda () (chipz:decompress nil 'chipz:zlib compressed-data-zlib)) 1)))
        
        (when (and (> lz4-compress-time 0) (> salza-compress-time 0))
          (format t "  LZ4 vs Zlib compression speed: ~,1Fx faster~%"
                  (/ salza-compress-time lz4-compress-time)))
        (when (and (> lz4-decompress-time 0) (> chipz-decompress-time 0))
          (format t "  LZ4 vs Zlib decompression speed: ~,1Fx faster~%"
                  (/ chipz-decompress-time lz4-decompress-time)))
        (when (and (plusp (length compressed-data-zlib)) (plusp (length compressed-data-lz4)))
            (format t "  LZ4 compression ratio vs Zlib: ~,1Fx larger~%"
                    (/ (length compressed-data-lz4) (length compressed-data-zlib))))))))

(defun run-scaling-benchmark ()
  "Test performance scaling with different data sizes"
  (format t "~&~%======================================================================~%")
  (format t "~&--- Scaling Benchmark (cl-lz4-rs only) ---~%")
  (format t "======================================================================~%")
  
  (dolist (size-kb '(1 10 100 1000))
    (let* ((size-mb (/ size-kb 1024.0))
           (data-size (* size-kb 1024))
           (iterations (max 1 (min 50 (floor 5000 size-kb))))
           (test-data (make-array data-size :element-type '(unsigned-byte 8))))
      
      ;; Fill with pattern that compresses reasonably well
      (dotimes (i data-size)
        (setf (aref test-data i) (mod (+ i (floor i 16)) 256)))
      
      (format t "~&~%Size: ~A KB (~,3F MB), ~D iterations~%" size-kb size-mb iterations)
      
      (let* ((compressed (compress test-data))
             (compress-time (simple-time-operation
                              (lambda () (compress test-data))
                              iterations))
             (decompress-time (simple-time-operation
                                (lambda () (decompress compressed data-size))
                                iterations)))
        
        (if (> compress-time 0)
            (format t "  Compression:   ~,2F MB/s (~,4F ms per op)~%" 
                    (/ size-mb compress-time) (* compress-time 1000))
            (format t "  Compression:   N/A (too fast to measure)~%"))
        (if (> decompress-time 0)
            (format t "  Decompression: ~,2F MB/s (~,4F ms per op)~%" 
                    (/ size-mb decompress-time) (* decompress-time 1000))
            (format t "  Decompression: N/A (too fast to measure)~%"))
        (format t "  Ratio:         ~,2F%% (~:D -> ~:D bytes)~%"
                (* 100.0 (/ (length compressed) data-size))
                data-size (length compressed))))))

(defun run-all-benchmarks ()
  "Run and report on all defined benchmarks for multiple data sizes."
  (format t "~&=== CL-LZ4-RS Benchmark Suite ===~%")
  
  ;; Standard comparison benchmarks
  (run-comparison-benchmarks 1 50)
  (run-comparison-benchmarks 10 10)
  
  ;; Scaling benchmark
  (run-scaling-benchmark)
  
  (format t "~&~%======================================================================~%")
  (format t "~&--- Benchmark Suite Complete ---~%")
  (format t "======================================================================~%"))


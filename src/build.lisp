(defpackage #:cl-lz4-rs/build
  (:use #:cl)
  (:export #:build-native-library
           #:ensure-native-library
           #:get-library-path
           #:clean-build-artifacts
           #:library-exists-p
           #:library-needs-rebuild-p
           #:build-status
           #:rust-installed-p
           #:auto-build-disabled-p))


(in-package #:cl-lz4-rs/build)


(defun get-library-name ()
  #+windows "lz4_wrapper.dll"
  #+darwin  "liblz4_wrapper.dylib"
  #+(and unix (not darwin)) "liblz4_wrapper.so"
  #-(or windows darwin (and unix (not darwin))) (error "Unsupported platform"))


(defun get-system-directory ()
  (asdf:system-source-directory :cl-lz4-rs))


(defun get-rust-target-dir ()
  (merge-pathnames "lz4_wrapper/target/release/" (get-system-directory)))


(defun get-library-source-path ()
  (merge-pathnames (get-library-name) (get-rust-target-dir)))


(defun get-library-dest-path ()
  (merge-pathnames (get-library-name) (get-system-directory)))


(defun get-library-path ()
  "Get the path where the library should be located."
  (get-library-dest-path))


(defun rust-installed-p ()
  (handler-case
      (progn
        (uiop:run-program
         (list #+windows "where" #-windows "which" "cargo")
         :ignore-error-status t
         :output :string)
        t)
    (error () nil)))


(defun library-exists-p ()
  "Check if the native library exists in the expected location."
  (probe-file (get-library-dest-path)))


(defun library-needs-rebuild-p ()
  "Check if the library needs to be rebuilt (doesn't exist or is older than Rust source)."
  (let ((lib-path (get-library-dest-path))
        (cargo-toml (merge-pathnames "lz4_wrapper/Cargo.toml" (get-system-directory)))
        (rust-src (merge-pathnames "lz4_wrapper/src/lib.rs" (get-system-directory))))
    (or (not (probe-file lib-path))
        (and (probe-file cargo-toml)
             (> (file-write-date cargo-toml) (file-write-date lib-path)))
        (and (probe-file rust-src)
             (> (file-write-date rust-src) (file-write-date lib-path))))))


(defun auto-build-disabled-p ()
  (uiop:getenvp "CL_LZ4_RS_NO_AUTO_BUILD"))


(defun build-native-library (&key force verbose)
  "Build the Rust native library. If FORCE is T, rebuild even if library exists."
  (let ((system-dir (get-system-directory))
        (dest-path (get-library-dest-path)))

    (when (and (not force) (library-exists-p) (not (library-needs-rebuild-p)))
      (when verbose
        (format t "~&[INFO] Native library is up to date at ~A~%" dest-path))
      (return-from build-native-library dest-path))

    (unless (rust-installed-p)
      (error "Rust toolchain not found. Please install Rust from https://rustup.rs/"))

    (format t "~&[INFO] Building Rust native library...~%")

    (let ((cargo-toml (merge-pathnames "lz4_wrapper/Cargo.toml" system-dir)))
      (unless (probe-file cargo-toml)
        (error "Cargo.toml not found at ~A. Please check your project structure." cargo-toml)))

    (handler-case
        (let ((output (if verbose :interactive :string))
              (error-output (if verbose :interactive :string)))
          (uiop:run-program
           (list "cargo" "build" "--release"
                 "--manifest-path" (namestring (merge-pathnames "lz4_wrapper/Cargo.toml" system-dir)))
           :directory system-dir
           :output output
           :error-output error-output))
      (uiop:subprocess-error (e)
        (error "Cargo build failed: ~A" e)))

    (let ((source-path (get-library-source-path)))
      (unless (probe-file source-path)
        (error "Built library not found at ~A. Build may have failed." source-path))

      (when verbose
        (format t "~&[INFO] Copying library from ~A to ~A~%" source-path dest-path))

      (uiop:copy-file source-path dest-path)

      (unless (probe-file dest-path)
        (error "Failed to copy library to ~A" dest-path))

      (format t "~&[OK] Native library built successfully at ~A~%" dest-path)
      dest-path)))


(defun ensure-native-library (&key verbose)
  "Ensure the native library exists, building it if necessary."
  (let ((dest-path (get-library-dest-path)))
    (cond
      ((library-exists-p)
       (when verbose
         (format t "~&[INFO] Native library found at ~A~%" dest-path))
       dest-path)
      (t
       (format t "~&[INFO] Native library not found, building automatically...~%")
       (build-native-library :verbose verbose)))))


(defun clean-build-artifacts ()
  (let ((system-dir (get-system-directory)))
    (format t "~&[INFO] Cleaning build artifacts...~%")

    (let ((lib-path (get-library-dest-path)))
      (when (probe-file lib-path)
        (delete-file lib-path)
        (format t "~&[INFO] Removed ~A~%" lib-path)))

    (handler-case
        (uiop:run-program
         (list "cargo" "clean" "--manifest-path"
               (namestring (merge-pathnames "lz4_wrapper/Cargo.toml" system-dir)))
         :directory system-dir
         :output :string
         :error-output :string)
      (error (e)
        (warn "Failed to run cargo clean: ~A" e)))

    (format t "~&[OK] Clean complete~%")))


(defmethod asdf:perform :before ((op asdf:load-op) (system (eql (asdf:find-system :cl-lz4-rs))))
  "Automatically ensure native library exists before loading the system, unless disabled."
  (unless (auto-build-disabled-p)
    (ensure-native-library :verbose t)))


(defmethod asdf:perform :before ((op asdf:compile-op) (component asdf:cl-source-file))
  "Ensure native library exists before compiling any component of cl-lz4-rs, unless disabled."
  (when (and (not (auto-build-disabled-p))
             (eq (asdf:component-system component) (asdf:find-system :cl-lz4-rs)))
    (ensure-native-library :verbose nil)))


(defun build-status ()
  (format t "~&--- CL-LZ4-RS Build Status ---~%")
  (format t "System directory:      ~A~%" (get-system-directory))
  (format t "Expected library:      ~A~%" (get-library-dest-path))
  (format t "Auto-build disabled:   ~A~%" (if (auto-build-disabled-p) "[OK] Yes (CL_LZ4_RS_NO_AUTO_BUILD is set)" "[INFO] No"))
  (format t "Library exists:        ~A~%" (if (library-exists-p) "[OK] Yes" "[FAIL] No"))
  (format t "Rust available:        ~A~%" (if (rust-installed-p) "[OK] Yes" "[FAIL] No"))
  (when (library-exists-p)
    (format t "Library modified:      ~A~%"
            (multiple-value-bind (sec min hour date month year)
                (decode-universal-time (file-write-date (get-library-dest-path)))
              (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                      year month date hour min sec))))
  (format t "Needs rebuild:         ~A~%" (if (library-needs-rebuild-p) "[WARN] Yes" "[OK] No")))

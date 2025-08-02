# cl-lz4-rs

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Quicklisp](https://img.shields.io/badge/Quicklisp-available-brightgreen.svg)](http://quicklisp.org/)

An incredibly fast LZ4 compression library for Common Lisp, using the safety and speed of Rust.
- Pure Lisp variant can be found [here.](https://github.com/ItsMeForLua/cl-lz4.git)
## Overview

`cl-lz4-rs` provides incredibly fast LZ4 compression and decompression for Common Lisp by wrapping the `lz4_flex` Rust crate via FFI. This brings massive performance gains over pure Lisp alternatives.

## Performance:

The performance gains from using Rust FFI are significant. In our benchmarks, `cl-lz4-rs` consistently outperforms other libraries.

**10MB Data Compression/Decompression:**

| **Library** | **Operation** | **Avg. Throughput** | **Relative Speed** |
| --- | --- | --- | --- |
| **cl-lz4-rs** | **Decompression** | **~1000 MB/s** | **~7x faster** |
| Chipz (zlib) | Decompression | ~142 MB/s | 1.0x |
|     |     |     |     |
| **cl-lz4-rs** | **Compression** | **(Too fast to measure)** | **~12x faster** |
| Salza2 (zlib) | Compression | ~77 MB/s | 1.0x |

*Your results may vary based on hardware*
## Features

- **Blazing Fast:** As shown above, achieve throughput of over 1 GB/s on modern hardware.
  
- **Automatic Build:** The required Rust library is compiled automatically on first load. No manual build steps are needed after cloning.
  
- **Memory Efficient:** Zero-copy data transfer between Lisp and Rust. Buffers are managed by the Lisp GC, preventing memory leaks.
  
- **Cross-Platform:** Works on Linux, macOS, and Windows.
  
- **Zero C Dependencies:** The underlying implementation is pure Rust, avoiding the need for a separate `liblz4` system dependency.
  

## Installation (for Users)

Once available in Quicklisp, you can load it with:

``` lisp
(ql:quickload :cl-lz4-rs)
```

Until then, clone the repository into your Quicklisp `local-projects` directory:

``` bash
cd ~/quicklisp/local-projects/
git clone https://github.com/ItsMeForLua/cl-lz4-rs.git
```

Then, simply load the system. The native library will be built automatically.

``` lisp
(ql:quickload :cl-lz4-rs)
```

## Usage Example

Reference the functions in your own code. All symbols are exported from the `cl-lz4-rs` package.

```  lisp
(use-package :cl-lz4-rs)

;; Create 1MB of sample data
(defparameter *original-data*
  (let ((vec (make-array (* 1024 1024) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length vec)
          do (setf (aref vec i) (mod i 256)))
    vec))

(defparameter *compressed* (compress *original-data*))
(defparameter *decompressed* (decompress *compressed* (length *original-data*)))

;; Verify the round-trip was successful
(format t "Round-trip successful: ~A~%" (equalp *original-data* *decompressed*))
;; => Round-trip successful: T
```

---


## Developer Setup (for Contributors)

This project uses **Qlot** for reproducible dependency management and a **Makefile** for automating common tasks.

### Prerequisites

- Install the [Rust Toolchain](https://rustup.rs/ "null").
  
- Install [Roswell](https://github.com/roswell/roswell/wiki "null") (for managing Lisp implementations).
  
- Install [Qlot](https://github.com/fukamachi/qlot "null") (`ros install qlot`).
  
- Install [Docker](https://www.docker.com/ "null") and [Docker Compose](https://docs.docker.com/compose/install/ "null") (for running the CI pipeline).
  

### Dependency Management with Qlot

1. **Install Project Dependencies**: This command reads the `qlfile` and installs the exact versions of all dependencies into a local `.qlot/` directory.
  
  ``` bash
  # From the project's root directory
  make deps
  ```
  
2. **Start a REPL**: To work on the project, start your REPL using `qlot exec`. This ensures your Lisp session uses the project's local dependencies.
  
  ``` bash
  qlot exec ros run
  ```
  

### Continuous Integration with Docker & Jenkins

The repository includes a complete CI setup to automate testing and ensure code quality.

- **`Dockerfile`**: Defines a build environment with Roswell, SBCL, Qlot, and the Rust toolchain pre-installed.
  
- **`docker-compose.yml` & `Dockerfile.jenkins`**: These files set up a local Jenkins instance, configured to run the project's pipeline.
  
- **`Jenkinsfile`**: This file defines the CI pipeline stages: build, test, and benchmark.
  

To run the full CI pipeline locally:

1. **Start the Jenkins server**:
  
  ``` bash
  # Ensure your user can access the Docker daemon
  docker-compose up -d
  ```
  
2. **Access Jenkins** at `http://localhost:8080`.
  
3. **Create a new "Pipeline" job** and configure it to use the `Jenkinsfile` from SCM (pointing to your local git repository). This will replicate the exact environment used for automated testing.

## Running Tests and Benchmarks

The `Makefile` provides the easiest way to run the test and benchmark suites.

- **To run the test suite**:
  
  ``` bash
  # Using the qlot environment
  make test
  
  # Or using your local system's Lisp environment
  make test-local
  ```
  
- **To run the benchmarks**:
  
  ``` bash
  # Using the qlot environment
  make benchmark
  
  # Or using your local system's Lisp environment
  make benchmark-local
  ```

## Contributing

Bug reports and pull requests are welcome on GitHub. Please ensure the test suite passes by atleast running `make test` before submitting a pull request. However, it is preferable you use the Jenkins CI/CD pipeline before submitting a PR, as this will prevent silly mistakes (happens to all of us), and will reduce the Github actions usage.

## License

This project is licensed under the **MIT License**. See the `LICENSE` file for details.

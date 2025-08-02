# Makefile for cl-lz4-rs

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    LIB_EXT = so
    LIB_PREFIX = lib
endif
ifeq ($(UNAME_S),Darwin)
    LIB_EXT = dylib
    LIB_PREFIX = lib
endif
ifdef OS
    ifeq ($(OS),Windows_NT)
        LIB_EXT = dll
        LIB_PREFIX =
    endif
endif

# Auto-detect Lisp environment, with fallback
LOCAL_PROJECTS_DIR ?= $(shell \
	if [ -d ~/.roswell/local-projects ]; then \
		echo ~/.roswell/local-projects; \
	elif [ -d ~/quicklisp/local-projects ]; then \
		echo ~/quicklisp/local-projects; \
	else \
		echo ~/.roswell/local-projects; \
	fi)

LIB_NAME = $(LIB_PREFIX)lz4_wrapper.$(LIB_EXT)
RUST_TARGET_DIR = lz4_wrapper/target/release
RUST_LIB_PATH = $(RUST_TARGET_DIR)/$(LIB_NAME)

# Default to 'ros run', which uses the user's Roswell default.
# Can be overridden, e.g., 'make LISP=sbcl' or 'make LISP="ros -L ccl-bin run"'
LISP ?= ros run
QLOT ?= $(shell which qlot || if [ -f "$(HOME)/.roswell/bin/qlot" ]; then echo "$(HOME)/.roswell/bin/qlot"; else echo ""; fi)

.PHONY: all build clean test test-local benchmark benchmark-local dev-setup dev-setup-local install-dev status help deps

all: build

# Build the Rust native library
build:
	@echo "--> Building Rust native library..."
	@cargo build --release --manifest-path lz4_wrapper/Cargo.toml
	@echo "--> Copying library to project root..."
	@cp $(RUST_LIB_PATH) ./$(LIB_NAME)
	@echo "Build complete: $(LIB_NAME)"

clean:
	@echo "--> Cleaning build artifacts..."
	@cargo clean --manifest-path lz4_wrapper/Cargo.toml
	@rm -f $(LIB_NAME)
	@rm -rf .qlot/
	@echo "Clean complete"

# Install Lisp dependencies using qlot (requires network)
deps:
	@if [ -z "$(QLOT)" ]; then \
		echo "[ERROR] qlot not found. Please install it (e.g., 'ros install qlot')."; \
		exit 1; \
	fi
	@echo "--> Installing Lisp dependencies with qlot..."
	@$(QLOT) install

# Run tests with qlot (Requires network for deps)
test: build deps
	@echo "--> Running tests with qlot..."
	@CL_LZ4_RS_NO_AUTO_BUILD=t $(QLOT) exec $(LISP) --non-interactive \
		--eval '(push *default-pathname-defaults* asdf:*central-registry*)' \
		--eval "(asdf:test-system :cl-lz4-rs-tests)"

# Run tests with system Lisp (Works offline)
test-local: build
	@echo "--> Running tests with local Roswell environment (no qlot)..."
	@CL_LZ4_RS_NO_AUTO_BUILD=t $(LISP) --non-interactive \
		--eval '(push *default-pathname-defaults* asdf:*central-registry*)' \
		--eval "(ql:quickload :fiveam)" \
		--eval "(asdf:test-system :cl-lz4-rs-tests)"

# Run benchmarks with qlot (Requires network for deps)
benchmark: build deps
	@echo "--> Running benchmarks with qlot..."
	@CL_LZ4_RS_NO_AUTO_BUILD=t $(QLOT) exec $(LISP) --non-interactive \
		--eval '(push *default-pathname-defaults* asdf:*central-registry*)' \
		--eval "(ql:quickload :cl-lz4-rs-benchmarks)" \
		--eval "(cl-lz4-rs-benchmarks:run-all-benchmarks)"

# Run benchmarks with system Lisp (Works offline)
benchmark-local: build
	@echo "--> Running benchmarks with local Roswell environment (no qlot)..."
	@CL_LZ4_RS_NO_AUTO_BUILD=t $(LISP) --non-interactive \
		--eval '(push *default-pathname-defaults* asdf:*central-registry*)' \
		--eval "(ql:quickload :cl-lz4-rs-benchmarks)" \
		--eval "(cl-lz4-rs-benchmarks:run-all-benchmarks)" \
		--eval "(uiop:quit)"

# Install to local-projects (for development)
install-dev: build
	@echo "--> Installing to local-projects..."
	@mkdir -p $(LOCAL_PROJECTS_DIR)
	@if [ -L $(LOCAL_PROJECTS_DIR)/cl-lz4-rs ]; then \
		echo "Removing existing symlink..."; \
		rm $(LOCAL_PROJECTS_DIR)/cl-lz4-rs; \
	fi
	@ln -sf $(PWD) $(LOCAL_PROJECTS_DIR)/cl-lz4-rs
	@echo "[OK] Installed to $(LOCAL_PROJECTS_DIR)/cl-lz4-rs"

# Development setup with qlot (Requires network)
dev-setup: deps build install-dev
	@echo ""
	@echo "Development setup complete!"

# Development setup with system Lisp (Works offline)
dev-setup-local: build install-dev
	@echo ""
	@echo "Local development setup complete!"
	@echo "NOTE: This setup uses your global Roswell/Quicklisp environment."

status:
	@echo "--- CL-LZ4-RS Status ---"
	@echo "Lisp command: $(LISP)"
	@$(LISP) --non-interactive --eval "(load \"build.lisp\")" --eval "(cl-lz4-rs/build:build-status)"

help:
	@echo "CL-LZ4-RS Build System"
	@echo ""
	@echo "== Common Commands =="
	@echo "  make test             - Run tests in a reproducible qlot environment (needs network)."
	@echo "  make test-local       - Run tests in your local Roswell environment (works offline)."
	@echo "  make benchmark        - Run benchmarks in a reproducible qlot environment (needs network)."
	@echo "  make benchmark-local  - Run benchmarks in your local Roswell environment (works offline)."
	@echo "  make dev-setup        - Complete setup using qlot (needs network)."
	@echo "  make dev-setup-local  - Complete setup using local environment (works offline)."
	@echo "  make clean            - Clean all build artifacts."
	@echo "  make status           - Check current project status."


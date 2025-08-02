FROM debian:bullseye-slim

# Set non-interactive frontend for package managers to avoid prompts during build.
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential \
    curl \
    git \
    sudo \
    unzip \
    ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# IMPORTANT
# Create a non-root user 'builder' and give it passwordless sudo.
RUN useradd -m -s /bin/bash builder && \
    echo "builder ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

# Switch
USER builder
WORKDIR /home/builder/app

ENV RUSTUP_HOME=/home/builder/.rustup \
    CARGO_HOME=/home/builder/.cargo
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

RUN curl -L https://raw.githubusercontent.com/roswell/roswell/master/scripts/install-for-ci.sh | sh

# IMPORTANT: Makes 'ros' available to the next RUN layer.
ENV PATH="/home/builder/.cargo/bin:/home/builder/.roswell/bin:${PATH}"

# We split these into separate RUN commands to leverage Docker's layer caching.
RUN ros setup
RUN ros install sbcl-bin
RUN ros install qlot

COPY --chown=builder:users . .

RUN make deps && make build

# Define a neutral default command.
# The Jenkinsfile will override this to run specific tasks like 'make test'.
CMD ["/bin/bash"]

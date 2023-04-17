# Choose a base image with QEMU support
FROM multiarch/ubuntu-core:x86-bionic

# Set the environment variables
ENV DEBIAN_FRONTEND=noninteractive

# Install necessary packages, QEMU user emulation, and MLton
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        qemu-user \
        gcc \
        libc6-dev \
        binutils \
        make \
        curl \
        ca-certificates \
        git \
        mlton && \
    rm -rf /var/lib/apt/lists/*

# Define the working directory
WORKDIR /app

# Copy your project files into the container (modify the path according to your project structure)
COPY . /app

# Build your Standard ML project with MLton (assuming you have a main file named "main.sml")
RUN mlton main.sml

# Set the compiled binary as the entry point for the container
CMD ["./main"]

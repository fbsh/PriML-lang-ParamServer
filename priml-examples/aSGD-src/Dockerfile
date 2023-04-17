# Choose a base Ubuntu image
FROM ubuntu:20.04

# Set the environment variables
ENV DEBIAN_FRONTEND=noninteractive

# Install necessary packages and MLton
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
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

# Build your Standard ML project with the Makefile
RUN make

# Set the compiled binary as the entry point for the container
CMD ["./main"]


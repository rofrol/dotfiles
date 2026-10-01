#!/bin/bash
# Build minimal Alpine Linux live ISO with WiFi driver (Broadcom BCM43224) and SSH
# Usage: ./build-minimal-alpine-iso.sh
# Prerequisites: Docker, ~2GB disk space

set -euo pipefail

BUILD_DIR="/tmp/alpine-custom-iso"
WIFI_SSID="YourSSID"
WIFI_PASS="YourPassword"
ISO_NAME="alpine-macbook-recovery.iso"

mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

cat > Dockerfile << 'EOF'
FROM alpine:latest

# Install required packages for ISO building
RUN apk add --no-cache \
    alpine-sdk \
    alpine-conf \
    xorriso \
    syslinux \
    mtools \
    dosfstools \
    curl \
    git

# Install WiFi driver build tools
RUN apk add --no-cache \
    linux-headers \
    gcc \
    make \
    libc-dev \
    dkms \
    b43-fwcutter

# Clone alpine-make-vm-image (tool do custom Alpine images)
RUN git clone https://github.com/alpinelinux/alpine-make-vm-image.git /opt/alpine-make-vm-image

WORKDIR /build
COPY . .

# Customize rootfs
RUN mkdir -p /tmp/rootfs && \
    # Install sshd
    apk add --root /tmp/rootfs --no-cache openssh && \
    # Install WiFi tools
    apk add --root /tmp/rootfs --no-cache wpa_supplicant wireless-tools && \
    # Install Broadcom driver
    apk add --root /tmp/rootfs --no-cache broadcom-wl linux-firmware-brcm

# Configure SSH
RUN echo "PermitRootLogin yes" >> /tmp/rootfs/etc/ssh/sshd_config && \
    echo "PasswordAuthentication yes" >> /tmp/rootfs/etc/ssh/sshd_config

# Configure WiFi auto-connect
RUN mkdir -p /tmp/rootfs/etc/wpa_supplicant && \
    cat > /tmp/rootfs/etc/wpa_supplicant/wpa_supplicant.conf << 'WIFI'
network={
    ssid="SSID_PLACEHOLDER"
    psk="PASSWORD_PLACEHOLDER"
    key_mgmt=WPA-PSK
}
WIFI

# Build custom ISO
RUN cd /opt/alpine-make-vm-image && \
    ./alpine-make-vm-image.sh \
    --packages="openssh wpa_supplicant wireless-tools broadcom-wl" \
    --hostname=alpine-macbook \
    /build/alpine-custom.qcow2

FROM scratch
COPY --from=0 /build/alpine-custom.qcow2 /
EOF

# Build Docker image
echo "Building Docker image..."
docker build -t alpine-iso-builder .

# Extract ISO from container
echo "Extracting ISO..."
docker run --rm -v "$BUILD_DIR:/output" alpine-iso-builder sh -c "cp /alpine-custom.qcow2 /output/"

echo "✅ Custom ISO ready: $BUILD_DIR/alpine-custom.qcow2"
echo "Next steps:"
echo "  1. Convert qcow2 to ISO or boot directly"
echo "  2. Or use dd to write to USB directly"

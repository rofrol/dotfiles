# Build Minimal Alpine Linux ISO with WiFi & SSH

## Option A: Quick (on macOS/Linux with tools)

### Requirements
- `xorriso` or `mkisofs`
- `7z` or `unzip`
- ~1 GB free disk space

### Steps

**1. Download Alpine Extended ISO**
```bash
cd /tmp
wget https://dl-cdn.alpinelinux.org/alpine/latest-stable/releases/x86_64/alpine-extended-latest-x86_64.iso
```

**2. Mount and customize**
```bash
mkdir -p /tmp/alpine-mount /tmp/alpine-work
sudo mount -o loop alpine-extended-latest-x86_64.iso /tmp/alpine-mount
cp -r /tmp/alpine-mount/* /tmp/alpine-work/

# Unmount
sudo umount /tmp/alpine-mount
```

**3. Create custom init script**
```bash
mkdir -p /tmp/alpine-work/etc/init.d
cat > /tmp/alpine-work/etc/init.d/wifi-ssh-init << 'EOF'
#!/bin/sh

# Enable SSH
/etc/init.d/sshd start

# Connect to WiFi
wpa_cli -p /var/run/wpa_supplicant add_network
wpa_cli -p /var/run/wpa_supplicant set_network 0 ssid '"YOUR_SSID"'
wpa_cli -p /var/run/wpa_supplicant set_network 0 psk '"YOUR_PASSWORD"'
wpa_cli -p /var/run/wpa_supplicant set_network 0 key_mgmt WPA-PSK
wpa_cli -p /var/run/wpa_supplicant enable_network 0

# Or simpler - use wpa_supplicant.conf
wpa_supplicant -B -i wlan0 -c /etc/wpa_supplicant/wpa_supplicant.conf
dhclient wlan0
EOF
chmod +x /tmp/alpine-work/etc/init.d/wifi-ssh-init
```

**4. Create WiFi config**
```bash
mkdir -p /tmp/alpine-work/etc/wpa_supplicant
cat > /tmp/alpine-work/etc/wpa_supplicant/wpa_supplicant.conf << 'EOF'
ctrl_interface=/var/run/wpa_supplicant
ctrl_interface_group=wheel
ap_scan=1

network={
    ssid="YOUR_SSID"
    psk="YOUR_PASSWORD"
    key_mgmt=WPA-PSK
}
EOF
chmod 600 /tmp/alpine-work/etc/wpa_supplicant/wpa_supplicant.conf
```

**5. Rebuild ISO**
```bash
cd /tmp/alpine-work
xorriso -as mkisofs \
    -R -J -V "Alpine-MacBook" \
    -b syslinux/isolinux.bin \
    -c syslinux/boot.cat \
    -no-emul-boot \
    -boot-load-size 4 \
    -boot-info-table \
    -o /tmp/alpine-macbook-custom.iso \
    .
```

**6. Write to USB**
```bash
sudo dd if=/tmp/alpine-macbook-custom.iso of=/dev/sdX bs=4M status=progress
sync
```

---

## Option B: Docker-based (reproducible)

```bash
# See build-minimal-alpine-iso.sh
./build-minimal-alpine-iso.sh
```

---

## Option C: Alpine Linux Livekit (newest, recommended)

Alpine provides official livekit tool:

```bash
git clone https://github.com/alpinelinux/alpine-linux-livekit.git
cd alpine-linux-livekit

# Customize
vi profiles/default/packages.txt
# Add: openssh wpa_supplicant broadcom-wl

vi profiles/default/root/etc/wpa_supplicant/wpa_supplicant.conf
# Add your WiFi config

# Build
make
```

---

## Boot and SSH

```bash
# On MacBook, boot from USB
# Press Option key during startup → select USB

# From another computer:
ssh -l root <alpine-ip>
# Default password: none (no password for root in Alpine live)
# Or set in livekit build

# Find IP:
# On Alpine terminal: ifconfig
# Or check WiFi auto-connect status: wpa_cli status
```

---

## ISO Size Comparison

| ISO | Size | Boot Time |
|-----|------|-----------|
| Alpine Extended | ~130 MB | ~10s |
| Linux Mint 22.3 | ~2 GB | ~30s |
| Ubuntu minimal | ~500 MB | ~15s |

---

## Troubleshooting

**WiFi not connecting:**
```bash
# On Alpine live:
modprobe wl
wpa_supplicant -i wlan0 -c /etc/wpa_supplicant/wpa_supplicant.conf -D nl80211,wext
dhclient wlan0
ip addr show wlan0
```

**SSH not starting:**
```bash
/etc/init.d/sshd start
netstat -tlnp | grep sshd
```

**Broadcom driver not loading:**
```bash
# Check available drivers
modprobe -l | grep b43
modprobe -l | grep brcm

# Try manually
modprobe b43
lsmod | grep b43
```

---

## Security Note

This ISO has **hardcoded WiFi password in plain text**. Use only for:
- Recovery/maintenance (local network only)
- One-time MacBook rescue
- Not for public/shared networks

For production: use WPA Enterprise or key-based auth.

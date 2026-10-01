#!/bin/bash
# Configure Alpine ISO for SSH key-based authentication only (no password)
# Run this during ISO customization or on live Alpine

set -euo pipefail

# Configuration
AUTHORIZED_KEYS_FILE="/etc/ssh/authorized_keys"
SSH_KEY="${1:-$HOME/.ssh/id_ed25519.pub}"
SSH_PORT="${2:-22222}"  # Custom port instead of default 22

if [ ! -f "$SSH_KEY" ]; then
    echo "Error: SSH key not found at $SSH_KEY"
    echo "Usage: $0 /path/to/your/public/key.pub [SSH_PORT]"
    echo ""
    echo "Examples:"
    echo "  $0 ~/.ssh/id_ed25519.pub        # Use default port 22222"
    echo "  $0 ~/.ssh/id_ed25519.pub 2222  # Use custom port 2222"
    echo ""
    echo "Generate key if needed:"
    echo "  ssh-keygen -t ed25519 -f ~/.ssh/id_ed25519 -N ''"
    exit 1
fi

echo "=== Configuring Alpine for SSH key-based auth ==="

# 1. Install OpenSSH if not already
if ! command -v sshd &>/dev/null; then
    echo "Installing openssh..."
    apk add --no-cache openssh
fi

# 2. Create root home if doesn't exist
mkdir -p /root/.ssh
chmod 700 /root/.ssh

# 3. Add public key to authorized_keys
echo "Adding SSH public key..."
cat "$SSH_KEY" >> /root/.ssh/authorized_keys
chmod 600 /root/.ssh/authorized_keys

# 4. Harden SSH config - disable password auth, only allow key-based
echo "Configuring SSH on port $SSH_PORT..."
cat > /etc/ssh/sshd_config << EOF
# SSH Hardened Configuration for Alpine Recovery ISO

# Custom port (not default 22)
Port $SSH_PORT

# Only allow key-based authentication
PasswordAuthentication no
PubkeyAuthentication yes
AuthorizedKeysFile .ssh/authorized_keys

# Disable root password login
PermitRootLogin prohibit-password

# Other hardening
PermitEmptyPasswords no
X11Forwarding no
MaxAuthTries 3
ClientAliveInterval 60
ClientAliveCountInterval 3

# Logging
SyslogFacility AUTH
LogLevel VERBOSE
EOF

# 5. Generate SSH host keys if needed
if [ ! -f /etc/ssh/ssh_host_ed25519_key ]; then
    echo "Generating SSH host keys..."
    ssh-keygen -A
fi

# 6. Enable sshd at boot
echo "Enabling SSH at boot..."
rc-update add sshd boot 2>/dev/null || echo "Already enabled"

# 7. Start SSH
echo "Starting SSH..."
/etc/init.d/sshd start 2>/dev/null || true

# 8. Verify
echo ""
echo "=== SSH Configuration Complete ==="
echo ""
echo "✅ SSH key-based auth enabled"
echo "❌ Password auth disabled"
echo "🔌 SSH Port: $SSH_PORT"
echo "📍 Authorized key: $(cat $SSH_KEY | awk '{print $NF}')"
echo ""
echo "Test login:"
echo "  ssh -i ~/.ssh/id_ed25519 -p $SSH_PORT root@<alpine-ip>"
echo ""
echo "Add to ~/.ssh/config for convenience:"
echo "  Host alpine-macbook"
echo "    HostName <alpine-ip>"
echo "    User root"
echo "    Port $SSH_PORT"
echo "    IdentityFile ~/.ssh/id_ed25519"
echo ""
echo "Then simply: ssh alpine-macbook"
echo ""

# Show current SSH status
echo "SSH Status:"
/etc/init.d/sshd status || echo "SSH not running (will start on next boot)"

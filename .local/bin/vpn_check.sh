#!/bin/bash
if ip addr show tun0 > /dev/null 2>&1; then
    echo "VPN: ON"
else
    echo "VPN: OFF"
fi

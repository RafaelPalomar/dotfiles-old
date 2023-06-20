#!/bin/bash

# To run this proper, you need to add a comand sudo in /et/sudoers

# Stop the VPN
if [ -f /tmp/vpn_pid ]; then
    sudo kill $(cat /tmp/vpn_pid)
    rm /tmp/vpn_pid
fi

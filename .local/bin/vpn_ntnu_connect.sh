#!/bin/bash

# To run this proper, you need to add a comand sudo in /et/sudoers

# VPN parameters
user="$(pass ntnu_vpn/username)"
password="$(pass ntnu_vpn/password)"
server="vpn.ntnu.no"

# Start the VPN
echo $password | sudo openconnect --user=$user --passwd-on-stdin $server &
echo $! > /tmp/vpn_pid

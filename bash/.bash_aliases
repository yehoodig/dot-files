# Opens the chrome os firewall to allow game streaming by Steam on desktop
alias allow-steam-streaming='
sudo iptables -A INPUT -p udp --dport 27036 -j ACCEPT; 
sudo iptables -A INPUT -p udp --dport 37804 -j ACCEPT; 
sudo iptables -A INPUT -p tcp --dport 37804 -j ACCEPT; 
sudo iptables -A INPUT -p tcp --dport 27036 -j ACCEPT; 
sudo iptables -I INPUT -p udp --dport 27031 -j ACCEPT; 
sudo iptables -I INPUT -p udp --dport 27036 -j ACCEPT; 
sudo iptables -I INPUT -p tcp --dport 27036 -j ACCEPT; 
sudo iptables -I INPUT -p tcp --dport 27037 -j ACCEPT;'

alias allow-NAS-piserver='sudo iptables -I INPUT 1 -p udp --source 192.168.1.140/255.255.255.0 --dport 1025:65535 -j ACCEPT'
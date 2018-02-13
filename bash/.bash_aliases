alias steamStream='
sudo iptables -A INPUT -p udp --dport 27036 -j ACCEPT; 
sudo iptables -A INPUT -p udp --dport 37804 -j ACCEPT; 
sudo iptables -A INPUT -p tcp --dport 37804 -j ACCEPT; 
sudo iptables -A INPUT -p tcp --dport 27036 -j ACCEPT; 
sudo iptables -I INPUT -p udp --dport 27031 -j ACCEPT; 
sudo iptables -I INPUT -p udp --dport 27036 -j ACCEPT; 
sudo iptables -I INPUT -p tcp --dport 27036 -j ACCEPT; 
sudo iptables -I INPUT -p tcp --dport 27037 -j ACCEPT;'

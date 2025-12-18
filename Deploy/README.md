## Deploy Server for Microbe Decoder
This directory contains scripts and configuration files to deploy the
Microbe Decoder server on an **Ubuntu** system.  It uses Docker, nginx, and
a Node.js launcher service.  

### 1. Set up files for server
Make directories for files.  In Ubuntu, run
```
sudo mkdir -p /srv/microbedecoder-deploy
sudo chown "$USER:$USER" /srv/microbedecoder-deploy
```

Copy files to `/srv/microbedecoder-deploy/`.  If files are located on a Windows
system, run 
```
cp -r /mnt/path/to/files/. /srv/microbedecoder-deploy/
sudo apt install -y dos2unix
dos2unix /srv/microbedecoder-deploy/scripts/*.sh
```
replacing `/mnt/path/to/files` with the actual path.

Check directory structure.  Run
```
ls -1 /srv/microbedecoder-deploy
```

and verify output is
`README.md`
`configs`
`launcher`
`scripts`
`systemd`


Set permissions for files.  Run
```
sudo chown -R "$USER:$USER" /srv/microbedecoder-deploy
chmod +x /srv/microbedecoder-deploy/scripts/*.sh
```	
	
### 3. Deploy and test the server locally
Run deployment script.  In Ubuntu, run
```
bash /srv/microbedecoder-deploy/scripts/deploy.sh
```
	
This will
- Install Docker, Node.js, and nginx
- Pull the Microbe Decoder Docker image
- Install and configure nginx
- Install the Node.js launcher dependencies
- Install and enable the `microbe-launcher` systemd service
- Start the launcher

Visit site.  In a browser (e.g., Chrome), navigate to
```
http://localhost/
```

### 4. Deploy and test the server for production
These steps require
-A dedicated server (virtual machine).  Running these commands on a
personal machine risks permanent misconfiguration.  Virtual machines are purchased
through a cloud service provider (e.g., Azure).
-IP address for the virtual machine (found via `curl -s https://api.ipify.org`)
-A registered domain name (e.g., microbe-decoder.org).  Domain names are purchased
through a domain service provider (e.g., namecheap.com).
-A DNS record pointing the domain name (e.g., microbe-decoder.org) to IP address 
for the virtual machine (e.g., 192.0.2.1).  This is configured through the domain 
and cloud service providers.
Requirements are satistied if app can be accessed at 
```
http://dev.microbe-decoder.org/
```

After satisfying requirements, install a firewall.  On the virtual machine, run
```
chmod +x /srv/microbedecoder-deploy/scripts/install_firewall.sh
sudo bash /srv/microbedecoder-deploy/scripts/install_firewall.sh
```

Install a security certificate.  On the virtual machine, run
```
chmod +x /srv/microbedecoder-deploy/scripts/install_tls_cert.sh
sudo bash /srv/microbedecoder-deploy/scripts/install_tls_cert.sh \
  dev.microbe-decoder.org \
  admin@microbe-decoder.org
```
This will
- Obtain a TLS certificate
- Add a `listen 443 ssl` server block
- Configure HTTPS automatically
- Optionally redirect HTTP → HTTPS

Visit site.  In a browser (e.g., Chrome), navigate to
```
https://dev.microbe-decoder.org/
```

### Appendix
Other commands

Restart the launcher (without reinstalling anything). In Ubuntu run
```
bash /srv/microbedecoder-deploy/scripts/restart.sh
```

Restart services.  In Ubuntu run
```
sudo systemctl restart microbe-launcher
sudo systemctl restart nginx
```

Check status.  In Ubuntu run
```
sudo systemctl status microbe-launcher
sudo systemctl status nginx
```

Check nginx access logs
```
sudo grep -E ' /inst-| /_get_target ' /var/log/nginx/access.log | tail -n 100
```

Check Shiny logs
```
sudo docker ps --format "table {{.Names}}\t{{.Image}}\t{{.Status}}\t{{.Ports}}"
sudo docker logs -f --tail 200 <container_name_or_id>
```

Check launcher logs
```
sudo journalctl -u microbe-launcher -f
```

See status of Docker containers
```
sudo docker stats
```

See status of age sweeper
```
systemctl status md-age-sweep.timer
systemctl list-timers | grep md-age-sweep
journalctl -u md-age-sweep.service --no-pager -n 50
```
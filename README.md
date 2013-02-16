
# The spawn pool for Zerg demo

This application is an integral part of Zerg demo. It gets initial connection
from nginx, spawn a new 'zergling' instance (see
https://github.com/maximk/zergling) and instructs nginx to proxy the client
connection to the newly spawned instance.

# How to setup nginx

nginx configuration file must have a block similar to this:

	server {
		listen 80;
		server_name zerg.yourdomain.com;

		# Suppress spawning a second instance just to return 404
		#
		location /favicon.ico {
			return 404;
		}

		# A catapult location to bypass X-Accel-Redirect limitation
		#
		location ~* ^/internal_redirect/(.*?)/(.*?)/(.*) {
			internal;
			set $redirect_host $1;
			set $redirect_port $2;
			set $redirect_uri $3;
			set $redirect_to http://$redirect_host:$redirect_port/$redirect_uri;
			proxy_pass $redirect_to;
		}

		location / {
			proxy_set_header X-Real-IP $remote_addr;
			proxy_set_header Host $http_host;
			
			# Put location of your 'spawningpool' application here
			#
			proxy_pass http://localhost:9001;
		}
	}

# How to run the application

The application recorgizes the following command-line flags:

dom0-host
: the address of the Dom0 running libvirtd. Defaults to "192.168.0.1".

addr-base
: the beginning of the address pool to assign to instances. Defaults to
 "192.168.0.100".

num-addrs
: the maximum number of concurrent instances to spawn (16).

num-conns
: the number of libvirtd connections to use (1).

Beware that libvirtd serialises all request. Thus have more than one connection
open may not be worthwhile.


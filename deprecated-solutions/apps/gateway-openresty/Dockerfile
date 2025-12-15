FROM openresty/openresty:1.25.3.1-alpine

WORKDIR /app

# Copy nginx and Lua configuration
COPY nginx.conf /usr/local/openresty/nginx/conf/nginx.conf
COPY conf.d/ /etc/nginx/conf.d/
COPY lua/ /app/lua/

# Expose HTTP port used by gateway-openresty
EXPOSE 8080

CMD ["/usr/local/openresty/bin/openresty", "-g", "daemon off;"]

# Router Dockerfile with gRPC Health stub
FROM erlang:26-alpine

WORKDIR /app

# Install tools: bash, curl, python3 for gRPC health server
RUN apk add --no-cache bash curl python3 py3-pip && \
    python3 -m pip install --no-cache-dir --break-system-packages grpcio grpcio-health-checking

# Fetch grpc_health_probe binary
RUN curl -fsSL -o /usr/local/bin/grpc_health_probe \
    https://github.com/grpc-ecosystem/grpc-health-probe/releases/download/v0.4.24/grpc_health_probe-linux-amd64 && \
    chmod +x /usr/local/bin/grpc_health_probe

# Copy minimal Python gRPC Health server
COPY infra/docker/router/health_server.py /app/health_server.py

COPY infra/docker/router/start.sh /app/start.sh
RUN chmod +x /app/start.sh
# Create non-root user
RUN addgroup -g 1000 beamline && \
    adduser -D -u 1000 -G beamline beamline && \
    chown -R beamline:beamline /app

USER beamline

EXPOSE 9000

# Start Python gRPC Health server (temporary stub until Erlang router is ready)
CMD ["sh", "-lc", "/app/start.sh"]




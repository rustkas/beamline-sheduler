import grpc
from concurrent import futures
from grpc_health.v1 import health, health_pb2, health_pb2_grpc

PORT = 9000

def serve():
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=2))
    servicer = health.HealthServicer()
    # Set global service status to SERVING
    servicer.set('', health_pb2.HealthCheckResponse.SERVING)
    health_pb2_grpc.add_HealthServicer_to_server(servicer, server)
    server.add_insecure_port(f'[::]:{PORT}')
    server.start()
    print(f'gRPC Health server listening on {PORT}')
    try:
        server.wait_for_termination()
    except KeyboardInterrupt:
        server.stop(0)

if __name__ == '__main__':
    serve()


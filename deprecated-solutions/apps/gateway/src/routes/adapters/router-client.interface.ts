import { RouteRequestDto } from '../dto/route-request.dto';
import { RouteDecisionDto } from '../dto/route-decision.dto';
import { MessageDto } from '../dto/message.dto';

/**
 * Router Client Interface
 * Defines contract for Router/NATS integration
 * Can be replaced with real implementation (gRPC/NATS client) later
 */
export interface IRouterClient {
  /**
   * Request route decision from Router service
   * @param routeRequest Route request with message and context
   * @returns Route decision with selected provider
   * @throws Error if router is unavailable or request is invalid
   */
  decide(routeRequest: RouteRequestDto): Promise<RouteDecisionDto>;

  /**
   * Publish message to NATS (optional, for async processing)
   * @param message Message to publish
   * @returns Acknowledgment with message ID
   * @throws Error if publish fails
   */
  publish?(
    message: MessageDto | ({ message_id?: string } & Record<string, unknown>),
  ): Promise<{ message_id: string; ack: boolean }>;

  connect?(): Promise<void>;
  disconnect?(): Promise<void>;
}

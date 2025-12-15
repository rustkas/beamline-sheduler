/**
 * Mock Router Client for testing
 * Simulates Router/NATS adapter behavior
 */

import { RouteRequestDto } from '../dto/route-request.dto';
import { RouteDecisionDto } from '../dto/route-decision.dto';

export class MockRouterClient {
  private decisions: Map<string, RouteDecisionDto> = new Map();
  private delay: number = 0;
  private shouldFail: boolean = false;
  private errorMessage: string = 'Router error';

  /**
   * Set artificial delay for simulating network latency
   */
  setDelay(ms: number): void {
    this.delay = ms;
  }

  /**
   * Configure mock to fail with error
   */
  setShouldFail(shouldFail: boolean, errorMessage?: string): void {
    this.shouldFail = shouldFail;
    if (errorMessage) {
      this.errorMessage = errorMessage;
    }
  }

  /**
   * Pre-configure decision for specific message
   */
  setDecision(messageId: string, decision: RouteDecisionDto): void {
    this.decisions.set(messageId, decision);
  }

  /**
   * Clear all pre-configured decisions
   */
  clearDecisions(): void {
    this.decisions.clear();
  }

  /**
   * Simulate router decision via NATS/gRPC
   */
  async decide(routeRequest: RouteRequestDto): Promise<RouteDecisionDto> {
    // Simulate delay
    if (this.delay > 0) {
      await new Promise((resolve) => setTimeout(resolve, this.delay));
    }

    // Simulate failure
    if (this.shouldFail) {
      throw new Error(this.errorMessage);
    }

    // Return pre-configured decision if exists
    const messageId = routeRequest.message.message_id;
    if (this.decisions.has(messageId)) {
      return this.decisions.get(messageId)!;
    }

    // Default decision based on policy/context
    const decision: RouteDecisionDto = this.generateDecision(routeRequest);
    this.decisions.set(messageId, decision);
    return decision;
  }

  /**
   * Generate decision based on request
   */
  private generateDecision(routeRequest: RouteRequestDto): RouteDecisionDto {
    const messageId = routeRequest.message.message_id;
    const policyId = routeRequest.policy_id || 'default';
    const context = routeRequest.context || {};
    const preferred = (routeRequest.metadata as any)?.preferred_provider_id as string | undefined;

    // Simulate sticky session or preferred provider
    if (preferred) {
      return {
        provider_id: preferred,
        reason: 'preferred',
        priority: 100,
        expected_latency_ms: 200,
        expected_cost: 0.001,
        metadata: { preferred_provider_id: preferred },
      };
    }
    if (context.user_id) {
      return {
        provider_id: 'provider-sticky',
        reason: 'sticky',
        priority: 100,
        expected_latency_ms: 200,
        expected_cost: 0.001,
        metadata: { session_key: context.user_id },
      };
    }

    // Simulate weighted distribution
    if (policyId.includes('weighted')) {
      return {
        provider_id: 'provider-weighted',
        reason: 'weighted',
        priority: 50,
        expected_latency_ms: 250,
        expected_cost: 0.002,
        metadata: { policy_id: policyId },
      };
    }

    // Default decision
    return {
      provider_id: 'provider-default',
      reason: 'default',
      priority: 50,
      expected_latency_ms: 300,
      expected_cost: 0.001,
      metadata: { message_id: messageId },
    };
  }
}

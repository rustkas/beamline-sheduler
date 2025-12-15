/**
 * Test fixtures for messages
 */

import { MessageDto } from '../../src/routes/dto/message.dto';
import { RouteRequestDto } from '../../src/routes/dto/route-request.dto';
import { RouteDecisionDto } from '../../src/routes/dto/route-decision.dto';

export const validMessage: MessageDto = {
  message_id: 'msg_fixture_001',
  tenant_id: 'tenant_fixture',
  message_type: 'chat',
  payload: '{"text": "Hello, world!"}',
};

export const messageWithOptionalFields: MessageDto = {
  message_id: 'msg_fixture_002',
  tenant_id: 'tenant_fixture',
  trace_id: 'trace_fixture_001',
  message_type: 'completion',
  payload: '{"prompt": "Hello"}',
  metadata: { source: 'api', version: '1.0' },
  timestamp_ms: 1704067200000,
};

export const validRouteRequest: RouteRequestDto = {
  message: validMessage,
  policy_id: 'default',
  context: {},
};

export const routeRequestWithSticky: RouteRequestDto = {
  message: validMessage,
  policy_id: 'sticky-policy',
  context: {
    user_id: 'user_fixture_001',
  },
};

export const routeRequestWithWeighted: RouteRequestDto = {
  message: validMessage,
  policy_id: 'weighted-policy',
  context: {},
};

export const validRouteDecision: RouteDecisionDto = {
  provider_id: 'anthropic',
  reason: 'weighted',
  priority: 50,
  expected_latency_ms: 250,
  expected_cost: 0.001,
  metadata: {},
};

export const stickyRouteDecision: RouteDecisionDto = {
  provider_id: 'provider-sticky',
  reason: 'sticky',
  priority: 100,
  expected_latency_ms: 200,
  expected_cost: 0.001,
  metadata: { session_key: 'user_fixture_001' },
};

export const fallbackRouteDecision: RouteDecisionDto = {
  provider_id: 'provider-fallback',
  reason: 'fallback',
  priority: 25,
  expected_latency_ms: 500,
  expected_cost: 0.002,
  metadata: { condition: 'all_providers_failed' },
};

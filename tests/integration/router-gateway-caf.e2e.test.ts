import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { NATSConnection } from '../helpers/nats-helper';
import { GatewayClient } from '../helpers/gateway-client';
import { CAFWorkerSimulator } from '../helpers/caf-worker-simulator';
import { DevStateHelper } from '../helpers/devstate-helper';

/**
 * E2E Integration Tests for Router-Gateway-CAF Workflow
 * 
 * Tests the complete flow: Gateway → Router → CAF → Router → Gateway
 * Validates CP4-LC integration requirements
 */
describe('Router-Gateway-CAF E2E Integration', () => {
  let nats: NATSConnection;
  let gateway: GatewayClient;
  let cafWorker: CAFWorkerSimulator;
  let devstate: DevStateHelper;

  beforeAll(async () => {
    // Initialize test infrastructure
    nats = new NATSConnection({
      servers: process.env.NATS_URL || 'nats://localhost:4222',
      timeout: 10000
    });
    await nats.connect();

    gateway = new GatewayClient({
      baseUrl: process.env.GATEWAY_URL || 'http://localhost:3000',
      apiKey: process.env.TEST_API_KEY || 'test-key'
    });

    cafWorker = new CAFWorkerSimulator(nats);
    devstate = new DevStateHelper();

    // Initialize state if it doesn't exist
    try {
      const currentState = await devstate.readState();
      // If state exists but is not at CP3-LC, advance it
      if (currentState.current_cp !== 'CP3-LC') {
        await devstate.advanceCheckpoint('CP3-LC', 'test');
      }
    } catch (error) {
      // State doesn't exist, initialize it
      await devstate.initializeState('beamline-constructor');
      // Set current checkpoint to CP3-LC
      await devstate.advanceCheckpoint('CP3-LC', 'test');
    }

    // Update worker agent status to completed to simulate ready state
    await devstate.updateAgentStatus('worker', 'completed');

    // Verify system is ready
    await devstate.verifyCheckpointCriteria({
      cp: 'CP3-LC',
      required_agents: ['worker'],
      min_tests_passed: 1,
      max_error_rate: 0.1,
      required_checksums: ['.trae/state.json']
    });
  });

  afterAll(async () => {
    await nats.disconnect();
  });

  describe('Basic Message Flow', () => {
    it('should complete full chat completion workflow', async () => {
      const startTime = Date.now();
      const traceId = `test-${startTime}`;
      const tenantId = 'test-tenant';

      // Step 1: Send request through Gateway
      const gatewayResponse = await gateway.chatCompletion({
        messages: [{ role: 'user', content: 'Hello, world!' }],
        model: 'gpt-4',
        traceId,
        tenantId
      });

      expect(gatewayResponse.status).toBe(200);
      expect(gatewayResponse.data.assignment_id).toBeDefined();

      // Step 2: Verify Router decision
      const decision = await nats.waitForMessage('beamline.router.v1.decide.reply', {
        trace_id: traceId,
        timeout: 5000
      });

      expect(decision.provider_id).toBeDefined();
      expect(decision.reason).toBeDefined();

      // Step 3: Verify CAF assignment
      const assignment = await nats.waitForMessage('caf.exec.assign.v1', {
        trace_id: traceId,
        timeout: 5000
      });

      expect(assignment.assignment_id).toBe(gatewayResponse.data.assignment_id);
      expect(assignment.tenant_id).toBe(tenantId);

      // Step 4: Simulate CAF processing
      const execResult = await cafWorker.processAssignment(assignment, {
        output: 'Hello! How can I help you today?',
        latency_ms: 150,
        cost: 0.002
      });

      // Step 5: Verify result delivery
      const finalResult = await gateway.waitForResult(gatewayResponse.data.assignment_id, {
        timeout: 10000
      });

      expect(finalResult.output).toBe('Hello! How can I help you today?');
      expect(finalResult.status).toBe('success');

      // Performance validation
      const totalTime = Date.now() - startTime;
      expect(totalTime).toBeLessThan(15000); // 15 second E2E SLA
    });

    it('should handle provider fallback on timeout', async () => {
      const traceId = `fallback-test-${Date.now()}`;
      
      // Configure policy with fallback
      await devstate.setPolicy('test-policy', {
        providers: [
          { name: 'slow-provider', weight: 70 },
          { name: 'fast-provider', weight: 30 }
        ],
        fallbacks: [{
          when: { status: ['timeout'] },
          retry: 1,
          to: 'fast-provider'
        }]
      });

      const response = await gateway.chatCompletion({
        messages: [{ role: 'user', content: 'Test fallback' }],
        policyId: 'test-policy',
        traceId
      });

      // Simulate slow provider timeout
      await cafWorker.simulateTimeout(response.data.assignment_id, 5000);

      // Verify fallback provider was used
      const finalResult = await gateway.waitForResult(response.data.assignment_id);
      expect(finalResult.provider_id).toBe('fast-provider');
    });
  });

  describe('Extension System Integration', () => {
    it('should process pre-processor extensions', async () => {
      const traceId = `pre-proc-test-${Date.now()}`;

      // Configure extension in registry
      await devstate.setExtension('normalize_text', {
        type: 'pre',
        subject: 'beamline.ext.pre.normalize_text.v1',
        timeout_ms: 100
      });

      // Add extension to policy
      await devstate.updatePolicy('test-policy', {
        pre: [{
          id: 'normalize_text',
          mode: 'required',
          config: { lowercase: true }
        }]
      });

      const response = await gateway.chatCompletion({
        messages: [{ role: 'user', content: 'HELLO WORLD' }],
        policyId: 'test-policy',
        traceId
      });

      // Verify extension was called
      const extRequest = await nats.waitForMessage('beamline.ext.pre.normalize_text.v1', {
        trace_id: traceId,
        timeout: 3000
      });

      expect(extRequest.message.payload).toBe('HELLO WORLD');
      expect(extRequest.extensions.config.lowercase).toBe(true);

      // Simulate extension response
      await nats.publish('beamline.ext.pre.normalize_text.v1.reply', {
        message: {
          ...extRequest.message,
          payload: 'hello world'
        },
        context: extRequest.context
      });

      const finalResult = await gateway.waitForResult(response.data.assignment_id);
      expect(finalResult.processed_text).toContain('hello world');
    });

    it('should validate with validator extensions', async () => {
      const traceId = `validator-test-${Date.now()}`;

      await devstate.setExtension('pii_guard', {
        type: 'validator',
        subject: 'beamline.ext.validate.pii_guard.v1',
        timeout_ms: 100
      });

      await devstate.updatePolicy('test-policy', {
        validators: [{
          id: 'pii_guard',
          on_fail: 'block'
        }]
      });

      // Test with PII content
      const response = await gateway.chatCompletion({
        messages: [{ role: 'user', content: 'My credit card is 1234-5678-9012-3456' }],
        policyId: 'test-policy',
        traceId
      });

      const extRequest = await nats.waitForMessage('beamline.ext.validate.pii_guard.v1', {
        trace_id: traceId
      });

      // Simulate PII detection
      await nats.publish('beamline.ext.validate.pii_guard.v1.reply', {
        status: 'reject',
        reason: 'pii_detected',
        details: { field: 'payload', pattern: 'credit_card' }
      });

      // Verify request was blocked
      await expect(gateway.waitForResult(response.data.assignment_id))
        .rejects.toThrow('Request blocked: pii_detected');
    });
  });

  describe('Error Handling and Resilience', () => {
    it('should handle NATS connection failures gracefully', async () => {
      // Simulate NATS disconnection
      await nats.simulateDisconnection();

      const response = await gateway.chatCompletion({
        messages: [{ role: 'user', content: 'Test resilience' }]
      });

      expect(response.status).toBe(503);
      expect(response.data.error).toContain('Service temporarily unavailable');

      // Restore connection
      await nats.reconnect();
    });

    it('should handle provider failures with retry', async () => {
      const traceId = `retry-test-${Date.now()}`;

      const response = await gateway.chatCompletion({
        messages: [{ role: 'user', content: 'Test retry' }],
        traceId
      });

      // Simulate provider failure
      await cafWorker.simulateFailure(response.data.assignment_id, 'provider_error');

      // Verify retry was attempted
      const retryResult = await gateway.waitForResult(response.data.assignment_id, {
        timeout: 15000
      });

      expect(retryResult.retry_count).toBeGreaterThan(0);
    });
  });

  describe('Performance and Throughput', () => {
    it('should handle concurrent requests efficiently', async () => {
      const concurrentRequests = 10;
      const requests = [];

      // Send concurrent requests
      for (let i = 0; i < concurrentRequests; i++) {
        requests.push(gateway.chatCompletion({
          messages: [{ role: 'user', content: `Concurrent request ${i}` }],
          traceId: `concurrent-${i}-${Date.now()}`
        }));
      }

      const responses = await Promise.all(requests);
      
      // Verify all requests were accepted
      expect(responses.every(r => r.status === 200)).toBe(true);

      // Wait for all results
      const results = await Promise.all(
        responses.map(r => gateway.waitForResult(r.data.assignment_id))
      );

      expect(results.every(r => r.status === 'success')).toBe(true);
    });

    it('should meet latency SLA requirements', async () => {
      const results = [];
      
      for (let i = 0; i < 5; i++) {
        const startTime = Date.now();
        
        const response = await gateway.chatCompletion({
          messages: [{ role: 'user', content: `Latency test ${i}` }]
        });

        const result = await gateway.waitForResult(response.data.assignment_id);
        const totalTime = Date.now() - startTime;
        
        results.push(totalTime);
      }

      const avgLatency = results.reduce((a, b) => a + b, 0) / results.length;
      expect(avgLatency).toBeLessThan(5000); // 5 second average SLA
    });
  });

  describe('Multi-tenancy and Isolation', () => {
    it('should isolate tenant data and resources', async () => {
      const tenant1 = 'tenant-1';
      const tenant2 = 'tenant-2';
      const traceId = `isolation-${Date.now()}`;

      // Configure different policies for each tenant
      await devstate.setPolicy('policy-1', { providers: [{ name: 'provider-a', weight: 100 }] }, tenant1);
      await devstate.setPolicy('policy-2', { providers: [{ name: 'provider-b', weight: 100 }] }, tenant2);

      // Send requests for both tenants
      const [response1, response2] = await Promise.all([
        gateway.chatCompletion({
          messages: [{ role: 'user', content: 'Tenant 1 request' }],
          tenantId: tenant1,
          policyId: 'policy-1',
          traceId: `${traceId}-1`
        }),
        gateway.chatCompletion({
          messages: [{ role: 'user', content: 'Tenant 2 request' }],
          tenantId: tenant2,
          policyId: 'policy-2',
          traceId: `${traceId}-2`
        })
      ]);

      const [result1, result2] = await Promise.all([
        gateway.waitForResult(response1.data.assignment_id),
        gateway.waitForResult(response2.data.assignment_id)
      ]);

      // Verify isolation
      expect(result1.provider_id).toBe('provider-a');
      expect(result2.provider_id).toBe('provider-b');
    });
  });

  describe('Observability and Monitoring', () => {
    it('should generate proper trace spans across components', async () => {
      const traceId = `trace-test-${Date.now()}`;

      await gateway.chatCompletion({
        messages: [{ role: 'user', content: 'Trace test' }],
        traceId
      });

      // Verify trace propagation
      const spans = await devstate.getTraceSpans(traceId);
      
      expect(spans).toContainEqual(expect.objectContaining({
        component: 'gateway',
        operation: 'chat_completion'
      }));
      
      expect(spans).toContainEqual(expect.objectContaining({
        component: 'router', 
        operation: 'route_decision'
      }));
      
      expect(spans).toContainEqual(expect.objectContaining({
        component: 'caf_worker',
        operation: 'process_assignment'
      }));
    });

    it('should emit correct metrics for monitoring', async () => {
      const metricsBefore = await devstate.getMetrics();
      
      await gateway.chatCompletion({
        messages: [{ role: 'user', content: 'Metrics test' }]
      });

      const metricsAfter = await devstate.getMetrics();
      
      // Verify request counter increased
      expect(metricsAfter.requests_total).toBeGreaterThan(metricsBefore.requests_total);
      
      // Verify latency histogram
      expect(metricsAfter.request_duration_seconds).toBeDefined();
      
      // Verify error rate
      expect(metricsAfter.errors_total).toBeGreaterThanOrEqual(metricsBefore.errors_total);
    });
  });
});
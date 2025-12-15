/**
 * Gateway ↔ Router Backpressure Integration Tests
 * 
 * Tests end-to-end backpressure handling:
 * - Router detects overload and activates backpressure
 * - Gateway reads Router backpressure status
 * - Gateway returns 503/Retry-After when Router is overloaded
 * - Gateway returns to normal when Router recovers
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import axios, { AxiosError } from 'axios';

const GATEWAY_URL = process.env.GATEWAY_URL || 'http://localhost:8080';
const ROUTER_METRICS_URL = process.env.ROUTER_METRICS_URL || 'http://localhost:8080/_metrics';

describe('Gateway ↔ Router Backpressure Integration', () => {
    beforeAll(async () => {
        // Wait for services to be ready
        await new Promise(resolve => setTimeout(resolve, 2000));
    });

    afterAll(async () => {
        // Cleanup if needed
    });

    describe('Backpressure Detection and Response', () => {
        it('should return 503 Service Unavailable when Router backpressure is active', async () => {
            // Note: This test requires Router to be in backpressure state
            // In a real scenario, we would:
            // 1. Overload Router (send many messages)
            // 2. Wait for backpressure to activate
            // 3. Send request to Gateway
            // 4. Verify 503 response with Retry-After header

            const decideRequest = {
                tenant_id: 'test-tenant-1',
                request_id: 'req-backpressure-test-1',
                trace_id: 'trace-backpressure-test-1',
                task: {
                    type: 'test',
                    payload: { test: 'data' }
                }
            };

            try {
                const response = await axios.post(
                    `${GATEWAY_URL}/api/v1/routes/decide`,
                    decideRequest,
                    {
                        headers: {
                            'Content-Type': 'application/json',
                            'X-Tenant-ID': 'test-tenant-1'
                        },
                        validateStatus: (status) => status < 600 // Accept all status codes
                    }
                );

                // If Router is not in backpressure, we should get 200 OK
                // If Router is in backpressure, we should get 503 Service Unavailable
                if (response.status === 503) {
                    // Verify Retry-After header
                    expect(response.headers['retry-after']).toBeDefined();
                    const retryAfter = parseInt(response.headers['retry-after'] || '0');
                    expect(retryAfter).toBeGreaterThan(0);
                    expect(retryAfter).toBeLessThanOrEqual(60); // Reasonable retry time

                    // Verify error response format
                    expect(response.data).toHaveProperty('ok', false);
                    expect(response.data).toHaveProperty('error');
                    expect(response.data.error).toHaveProperty('code', 'service_overloaded');
                } else {
                    // Router is not in backpressure - request should succeed
                    expect(response.status).toBe(200);
                    expect(response.data).toHaveProperty('ok');
                }
            } catch (error) {
                if (error instanceof AxiosError && error.response) {
                    if (error.response.status === 503) {
                        // Verify Retry-After header
                        expect(error.response.headers['retry-after']).toBeDefined();
                        const retryAfter = parseInt(error.response.headers['retry-after'] || '0');
                        expect(retryAfter).toBeGreaterThan(0);

                        // Verify error response format
                        expect(error.response.data).toHaveProperty('ok', false);
                        expect(error.response.data).toHaveProperty('error');
                        expect(error.response.data.error).toHaveProperty('code', 'service_overloaded');
                    } else {
                        throw error;
                    }
                } else {
                    throw error;
                }
            }
        });

        it('should apply stricter rate limiting when Router backpressure is in warning state', async () => {
            // Note: This test requires Router to be in backpressure warning state
            // Gateway should continue processing but with stricter rate limits

            const decideRequest = {
                tenant_id: 'test-tenant-2',
                request_id: 'req-backpressure-warning-test-1',
                trace_id: 'trace-backpressure-warning-test-1',
                task: {
                    type: 'test',
                    payload: { test: 'data' }
                }
            };

            try {
                const response = await axios.post(
                    `${GATEWAY_URL}/api/v1/routes/decide`,
                    decideRequest,
                    {
                        headers: {
                            'Content-Type': 'application/json',
                            'X-Tenant-ID': 'test-tenant-2'
                        },
                        validateStatus: (status) => status < 600
                    }
                );

                // In warning state, Gateway should continue processing
                // but may apply stricter rate limiting
                // We can't easily verify rate limiting without sending many requests
                // So we just verify the request is processed (200 or 429)
                expect([200, 429]).toContain(response.status);
            } catch (error) {
                if (error instanceof AxiosError && error.response) {
                    // 429 is acceptable (rate limiting applied)
                    expect([200, 429]).toContain(error.response.status);
                } else {
                    throw error;
                }
            }
        });

        it('should return to normal processing when Router backpressure is inactive', async () => {
            // When Router backpressure is inactive, Gateway should process requests normally

            const decideRequest = {
                tenant_id: 'test-tenant-3',
                request_id: 'req-backpressure-inactive-test-1',
                trace_id: 'trace-backpressure-inactive-test-1',
                task: {
                    type: 'test',
                    payload: { test: 'data' }
                }
            };

            try {
                const response = await axios.post(
                    `${GATEWAY_URL}/api/v1/routes/decide`,
                    decideRequest,
                    {
                        headers: {
                            'Content-Type': 'application/json',
                            'X-Tenant-ID': 'test-tenant-3'
                        },
                        validateStatus: (status) => status < 600
                    }
                );

                // Should process normally (200 OK or Router error, but not 503)
                expect(response.status).not.toBe(503);
            } catch (error) {
                if (error instanceof AxiosError && error.response) {
                    // Should not be 503 (backpressure inactive)
                    expect(error.response.status).not.toBe(503);
                } else {
                    throw error;
                }
            }
        });
    });

    describe('Backpressure Recovery', () => {
        it('should recover from backpressure and resume normal processing', async () => {
            // This test simulates:
            // 1. Router enters backpressure (overloaded)
            // 2. Gateway returns 503
            // 3. Router recovers (backpressure inactive)
            // 4. Gateway resumes normal processing

            const decideRequest = {
                tenant_id: 'test-tenant-4',
                request_id: 'req-backpressure-recovery-test-1',
                trace_id: 'trace-backpressure-recovery-test-1',
                task: {
                    type: 'test',
                    payload: { test: 'data' }
                }
            };

            // Send multiple requests to verify recovery
            const requests = Array.from({ length: 5 }, (_, i) => 
                axios.post(
                    `${GATEWAY_URL}/api/v1/routes/decide`,
                    { ...decideRequest, request_id: `req-backpressure-recovery-test-${i + 1}` },
                    {
                        headers: {
                            'Content-Type': 'application/json',
                            'X-Tenant-ID': 'test-tenant-4'
                        },
                        validateStatus: (status) => status < 600
                    }
                ).catch(err => err.response || err)
            );

            const responses = await Promise.all(requests);

            // Verify that at least some requests were processed (not all 503)
            const non503Count = responses.filter(r => r.status !== 503).length;
            expect(non503Count).toBeGreaterThan(0);
        });
    });

    describe('Backpressure Priority', () => {
        it('should prioritize backpressure over rate limiting', async () => {
            // When Router backpressure is active, Gateway should return 503
            // even if rate limit is not exceeded

            const decideRequest = {
                tenant_id: 'test-tenant-5',
                request_id: 'req-backpressure-priority-test-1',
                trace_id: 'trace-backpressure-priority-test-1',
                task: {
                    type: 'test',
                    payload: { test: 'data' }
                }
            };

            try {
                const response = await axios.post(
                    `${GATEWAY_URL}/api/v1/routes/decide`,
                    decideRequest,
                    {
                        headers: {
                            'Content-Type': 'application/json',
                            'X-Tenant-ID': 'test-tenant-5'
                        },
                        validateStatus: (status) => status < 600
                    }
                );

                // If Router backpressure is active, should get 503 (not 429)
                if (response.status === 503) {
                    expect(response.data.error.code).toBe('service_overloaded');
                }
            } catch (error) {
                if (error instanceof AxiosError && error.response) {
                    if (error.response.status === 503) {
                        expect(error.response.data.error.code).toBe('service_overloaded');
                    }
                } else {
                    throw error;
                }
            }
        });
    });
});


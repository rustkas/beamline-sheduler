#!/usr/bin/env node
/**
 * Test Provider Extension
 * Subject: beamline.provider.test_provider.v1
 * 
 * Simple test provider that returns mock responses
 */

import { connect, StringCodec } from 'nats';

const SUBJECT = 'beamline.provider.test_provider.v1';
const NATS_URL = process.env.NATS_URL || 'nats://localhost:4222';

const sc = StringCodec();

async function main() {
    console.log(`[test_provider] Connecting to NATS: ${NATS_URL}`);
    
    try {
        const nc = await connect({ servers: NATS_URL });
        console.log(`[test_provider] Connected to NATS`);
        console.log(`[test_provider] Subscribing to: ${SUBJECT}`);
        
        const sub = nc.subscribe(SUBJECT);
        
        (async () => {
            for await (const msg of sub) {
                try {
                    const requestJson = sc.decode(msg.data);
                    const request = JSON.parse(requestJson);
                    
                    console.log(`[test_provider] Received request:`, JSON.stringify(request, null, 2));
                    
                    // Extract prompt/input
                    const prompt = request.prompt || request.body || '';
                    
                    // Generate mock response
                    const output = `Mock response to: ${prompt}`;
                    
                    // Build response (CP2-style)
                    const response = {
                        provider_id: 'test_provider',
                        output: output,
                        usage: {
                            prompt_tokens: prompt.length / 4,  // Rough estimate
                            completion_tokens: output.length / 4,
                            total_tokens: (prompt.length + output.length) / 4
                        },
                        metadata: {
                            source: 'test_provider',
                            model: 'test-model-v1'
                        }
                    };
                    
                    const responseJson = JSON.stringify(response);
                    console.log(`[test_provider] Sending response:`, responseJson);
                    
                    // Reply to request
                    if (msg.reply) {
                        msg.respond(sc.encode(responseJson));
                    }
                } catch (error) {
                    console.error(`[test_provider] Error processing request:`, error);
                    if (msg.reply) {
                        msg.respond(sc.encode(JSON.stringify({
                            provider_id: 'test_provider',
                            status: 'error',
                            error_code: 'PROCESSING_ERROR',
                            error_message: error.message
                        })));
                    }
                }
            }
        })().catch(err => {
            console.error(`[test_provider] Subscription error:`, err);
        });
        
        console.log(`[test_provider] Listening on ${SUBJECT}...`);
        
        // Graceful shutdown
        process.on('SIGINT', async () => {
            console.log(`[test_provider] Shutting down...`);
            await nc.close();
            process.exit(0);
        });
        
        process.on('SIGTERM', async () => {
            console.log(`[test_provider] Shutting down...`);
            await nc.close();
            process.exit(0);
        });
        
    } catch (error) {
        console.error(`[test_provider] Failed to connect:`, error);
        process.exit(1);
    }
}

main();


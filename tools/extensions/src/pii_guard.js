#!/usr/bin/env node
/**
 * PII Guard Validator Extension
 * Subject: beamline.ext.validate.pii_guard.v1
 * 
 * Validates that message doesn't contain PII (credit cards, SSN, etc.)
 */

import { connect, StringCodec } from 'nats';

const SUBJECT = 'beamline.ext.validate.pii_guard.v1';
const NATS_URL = process.env.NATS_URL || 'nats://localhost:4222';

const sc = StringCodec();

// Simple PII patterns (for testing)
const PII_PATTERNS = [
    /\b\d{4}[-\s]?\d{4}[-\s]?\d{4}[-\s]?\d{4}\b/,  // Credit card
    /\b\d{3}-\d{2}-\d{4}\b/,  // SSN
    /\b\d{3}\.\d{3}\.\d{4}\b/,  // SSN with dots
];

function detectPII(text) {
    if (typeof text !== 'string') {
        return null;
    }
    
    for (const pattern of PII_PATTERNS) {
        const match = text.match(pattern);
        if (match) {
            return {
                pattern: pattern.toString(),
                match: match[0]
            };
        }
    }
    
    return null;
}

async function main() {
    console.log(`[pii_guard] Connecting to NATS: ${NATS_URL}`);
    
    try {
        const nc = await connect({ servers: NATS_URL });
        console.log(`[pii_guard] Connected to NATS`);
        console.log(`[pii_guard] Subscribing to: ${SUBJECT}`);
        
        const sub = nc.subscribe(SUBJECT);
        
        (async () => {
            for await (const msg of sub) {
                try {
                    const requestJson = sc.decode(msg.data);
                    const request = JSON.parse(requestJson);
                    
                    console.log(`[pii_guard] Received request:`, JSON.stringify(request, null, 2));
                    
                    // Extract payload
                    const payload = request.payload || {};
                    const messagePayload = payload.payload || '';
                    
                    // Check for PII
                    const piiDetected = detectPII(messagePayload);
                    
                    // Build response
                    let response;
                    if (piiDetected) {
                        response = {
                            status: 'reject',
                            reason: 'pii_detected',
                            details: {
                                field: 'payload',
                                pattern: piiDetected.pattern,
                                match: piiDetected.match
                            }
                        };
                    } else {
                        response = {
                            status: 'ok'
                        };
                    }
                    
                    const responseJson = JSON.stringify(response);
                    console.log(`[pii_guard] Sending response:`, responseJson);
                    
                    // Reply to request
                    if (msg.reply) {
                        msg.respond(sc.encode(responseJson));
                    }
                } catch (error) {
                    console.error(`[pii_guard] Error processing request:`, error);
                    if (msg.reply) {
                        msg.respond(sc.encode(JSON.stringify({
                            status: 'reject',
                            reason: 'processing_error',
                            details: {
                                error: error.message
                            }
                        })));
                    }
                }
            }
        })().catch(err => {
            console.error(`[pii_guard] Subscription error:`, err);
        });
        
        console.log(`[pii_guard] Listening on ${SUBJECT}...`);
        
        // Graceful shutdown
        process.on('SIGINT', async () => {
            console.log(`[pii_guard] Shutting down...`);
            await nc.close();
            process.exit(0);
        });
        
        process.on('SIGTERM', async () => {
            console.log(`[pii_guard] Shutting down...`);
            await nc.close();
            process.exit(0);
        });
        
    } catch (error) {
        console.error(`[pii_guard] Failed to connect:`, error);
        process.exit(1);
    }
}

main();


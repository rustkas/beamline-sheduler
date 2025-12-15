#!/usr/bin/env node
/**
 * Mask PII Post-processor Extension
 * Subject: beamline.ext.post.mask_pii.v1
 * 
 * Masks PII in response text
 */

import { connect, StringCodec } from 'nats';

const SUBJECT = 'beamline.ext.post.mask_pii.v1';
const NATS_URL = process.env.NATS_URL || 'nats://localhost:4222';

const sc = StringCodec();

// Simple PII masking (for testing)
function maskPII(text) {
    if (typeof text !== 'string') {
        return text;
    }
    
    // Mask credit cards
    let masked = text.replace(/\b\d{4}[-\s]?\d{4}[-\s]?\d{4}[-\s]?\d{4}\b/g, '[CARD_MASKED]');
    
    // Mask SSN
    masked = masked.replace(/\b\d{3}[-\s.]?\d{2}[-\s.]?\d{4}\b/g, '[SSN_MASKED]');
    
    return masked;
}

async function main() {
    console.log(`[mask_pii] Connecting to NATS: ${NATS_URL}`);
    
    try {
        const nc = await connect({ servers: NATS_URL });
        console.log(`[mask_pii] Connected to NATS`);
        console.log(`[mask_pii] Subscribing to: ${SUBJECT}`);
        
        const sub = nc.subscribe(SUBJECT);
        
        (async () => {
            for await (const msg of sub) {
                try {
                    const requestJson = sc.decode(msg.data);
                    const request = JSON.parse(requestJson);
                    
                    console.log(`[mask_pii] Received request:`, JSON.stringify(request, null, 2));
                    
                    // Extract payload (response from provider)
                    const payload = request.payload || {};
                    const responsePayload = payload.payload || '';
                    
                    // Mask PII
                    const masked = maskPII(responsePayload);
                    
                    // Build response
                    const response = {
                        payload: {
                            ...payload,
                            payload: masked,
                            metadata: {
                                ...(payload.metadata || {}),
                                pii_masked: 'true'
                            }
                        },
                        metadata: {
                            ...(request.metadata || {})
                        }
                    };
                    
                    const responseJson = JSON.stringify(response);
                    console.log(`[mask_pii] Sending response:`, responseJson);
                    
                    // Reply to request
                    if (msg.reply) {
                        msg.respond(sc.encode(responseJson));
                    }
                } catch (error) {
                    console.error(`[mask_pii] Error processing request:`, error);
                    if (msg.reply) {
                        msg.respond(sc.encode(JSON.stringify({
                            error: {
                                code: 'PROCESSING_ERROR',
                                message: error.message
                            }
                        })));
                    }
                }
            }
        })().catch(err => {
            console.error(`[mask_pii] Subscription error:`, err);
        });
        
        console.log(`[mask_pii] Listening on ${SUBJECT}...`);
        
        // Graceful shutdown
        process.on('SIGINT', async () => {
            console.log(`[mask_pii] Shutting down...`);
            await nc.close();
            process.exit(0);
        });
        
        process.on('SIGTERM', async () => {
            console.log(`[mask_pii] Shutting down...`);
            await nc.close();
            process.exit(0);
        });
        
    } catch (error) {
        console.error(`[mask_pii] Failed to connect:`, error);
        process.exit(1);
    }
}

main();


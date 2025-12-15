#!/usr/bin/env node
/**
 * Normalize Text Pre-processor Extension
 * Subject: beamline.ext.pre.normalize_text.v1
 * 
 * Normalizes text: lowercase, trim, remove extra spaces
 */

import { connect, StringCodec } from 'nats';

const SUBJECT = 'beamline.ext.pre.normalize_text.v1';
const NATS_URL = process.env.NATS_URL || 'nats://localhost:4222';

const sc = StringCodec();

async function main() {
    console.log(`[normalize_text] Connecting to NATS: ${NATS_URL}`);
    
    try {
        const nc = await connect({ servers: NATS_URL });
        console.log(`[normalize_text] Connected to NATS`);
        console.log(`[normalize_text] Subscribing to: ${SUBJECT}`);
        
        const sub = nc.subscribe(SUBJECT);
        
        (async () => {
            for await (const msg of sub) {
                try {
                    const requestJson = sc.decode(msg.data);
                    const request = JSON.parse(requestJson);
                    
                    console.log(`[normalize_text] Received request:`, JSON.stringify(request, null, 2));
                    
                    // Extract payload
                    const payload = request.payload || {};
                    const messagePayload = payload.payload || '';
                    
                    // Normalize text
                    const normalized = typeof messagePayload === 'string' 
                        ? messagePayload.toLowerCase().trim().replace(/\s+/g, ' ')
                        : messagePayload;
                    
                    // Build response
                    const response = {
                        payload: {
                            ...payload,
                            payload: normalized,
                            metadata: {
                                ...(payload.metadata || {}),
                                normalized: 'true'
                            }
                        },
                        metadata: {
                            ...(request.metadata || {}),
                            detected_lang: 'en'
                        }
                    };
                    
                    const responseJson = JSON.stringify(response);
                    console.log(`[normalize_text] Sending response:`, responseJson);
                    
                    // Reply to request
                    if (msg.reply) {
                        msg.respond(sc.encode(responseJson));
                    }
                } catch (error) {
                    console.error(`[normalize_text] Error processing request:`, error);
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
            console.error(`[normalize_text] Subscription error:`, err);
        });
        
        console.log(`[normalize_text] Listening on ${SUBJECT}...`);
        
        // Graceful shutdown
        process.on('SIGINT', async () => {
            console.log(`[normalize_text] Shutting down...`);
            await nc.close();
            process.exit(0);
        });
        
        process.on('SIGTERM', async () => {
            console.log(`[normalize_text] Shutting down...`);
            await nc.close();
            process.exit(0);
        });
        
    } catch (error) {
        console.error(`[normalize_text] Failed to connect:`, error);
        process.exit(1);
    }
}

main();


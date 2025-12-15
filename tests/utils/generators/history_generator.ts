/**
 * History generator utilities for testing
 * Generates valid and invalid history JSON for testing
 */

import * as crypto from 'crypto';

export interface HistoryEntryOptions {
  ts?: string;
  actor?: string;
  action?: string;
  cp_from?: string | null;
  cp_to?: string;
  state_checksum?: string;
  hmac_prev?: string;
  secret?: string;
}

/**
 * Calculate HMAC for history entry
 */
function calculateHMAC(
  secret: string,
  ts: string,
  actor: string,
  action: string,
  stateChecksum: string,
  hmacPrev: string
): string {
  const data = `${ts}${actor}${action}${stateChecksum}${hmacPrev}`;
  return crypto.createHmac('sha256', secret).update(data).digest('hex');
}

/**
 * Generate valid history entry
 */
export function generateHistoryEntry(
  options: HistoryEntryOptions = {},
  secret: string = 'test-secret'
): any {
  const ts = options.ts || new Date().toISOString();
  const actor = options.actor || 'AGENT_1_REPO_STATE';
  const action = options.action || 'test_action';
  const cp_from = options.cp_from !== undefined ? options.cp_from : null;
  const cp_to = options.cp_to || 'CP0-LC';
  const state_checksum = options.state_checksum || crypto.randomBytes(32).toString('hex');
  const hmac_prev = options.hmac_prev !== undefined ? options.hmac_prev : '';
  
  const hmac = calculateHMAC(secret, ts, actor, action, state_checksum, hmac_prev);
  
  return {
    ts,
    actor,
    action,
    cp_from,
    cp_to,
    state_checksum,
    hmac_prev,
    hmac,
    metadata: {}
  };
}

/**
 * Generate valid history chain
 */
export function generateValidHistory(
  count: number = 1,
  secret: string = 'test-secret'
): any[] {
  const history: any[] = [];
  let prevHmac = '';
  
  for (let i = 0; i < count; i++) {
    const entry = generateHistoryEntry(
      {
        action: `action_${i}`,
        cp_from: i === 0 ? null : 'CP0-LC',
        cp_to: 'CP0-LC',
        hmac_prev: prevHmac
      },
      secret
    );
    
    history.push(entry);
    prevHmac = entry.hmac;
  }
  
  return history;
}

/**
 * Generate history with broken HMAC chain
 */
export function generateBrokenHistoryChain(
  secret: string = 'test-secret'
): any[] {
  const history = generateValidHistory(2, secret);
  
  // Break the chain by changing hmac_prev in second entry
  history[1].hmac_prev = 'broken_hmac_prev';
  history[1].hmac = calculateHMAC(
    secret,
    history[1].ts,
    history[1].actor,
    history[1].action,
    history[1].state_checksum,
    'broken_hmac_prev'
  );
  
  return history;
}

/**
 * Generate history with invalid entry structure
 */
export function generateInvalidHistoryEntry(): any {
  return {
    ts: new Date().toISOString(),
    actor: 'AGENT_1_REPO_STATE'
    // Missing required fields
  };
}


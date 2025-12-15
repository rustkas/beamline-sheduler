import { connect, JSONCodec, Subscription, Msg } from 'nats';
import { v4 as uuidv4 } from 'uuid';

/**
 * NATS Connection Helper for Integration Tests
 * Provides utilities for testing NATS message flows
 */
export class NATSConnection {
  private nc: any;
  private jc = JSONCodec();
  private subscriptions: Map<string, Subscription> = new Map();
  private messageLog: Map<string, any[]> = new Map();

  constructor(private config: { servers: string; timeout?: number }) {}

  async connect(): Promise<void> {
    this.nc = await connect({
      servers: this.config.servers,
      timeout: this.config.timeout || 10000
    });
  }

  async disconnect(): Promise<void> {
    // Clean up subscriptions
    for (const sub of this.subscriptions.values()) {
      sub.unsubscribe();
    }
    this.subscriptions.clear();

    if (this.nc) {
      await this.nc.drain();
      await this.nc.close();
    }
  }

  /**
   * Publish message to NATS subject
   */
  async publish(subject: string, data: any): Promise<void> {
    if (!this.nc) throw new Error('NATS not connected');
    
    const payload = this.jc.encode(data);
    await this.nc.publish(subject, payload);
  }

  /**
   * Subscribe to NATS subject and collect messages
   */
  subscribe(subject: string, opts?: { queue?: string }): Subscription {
    if (!this.nc) throw new Error('NATS not connected');

    const sub = this.nc.subscribe(subject, {
      queue: opts?.queue,
      callback: (err: Error | null, msg: Msg) => {
        if (err) {
          console.error(`NATS subscription error on ${subject}:`, err);
          return;
        }

        try {
          const data = this.jc.decode(msg.data);
          
          // Store message for later retrieval
          if (!this.messageLog.has(subject)) {
            this.messageLog.set(subject, []);
          }
          this.messageLog.get(subject)!.push({
            data,
            headers: msg.headers,
            timestamp: Date.now()
          });

          // Log for debugging
          console.log(`NATS message on ${subject}:`, JSON.stringify(data, null, 2));
        } catch (decodeError) {
          console.error(`Failed to decode NATS message on ${subject}:`, decodeError);
        }
      }
    });

    this.subscriptions.set(subject, sub);
    return sub;
  }

  /**
   * Wait for message matching criteria
   */
  async waitForMessage(subject: string, criteria: {
    trace_id?: string;
    assignment_id?: string;
    timeout?: number;
  }): Promise<any> {
    const timeout = criteria.timeout || 5000;
    const startTime = Date.now();

    return new Promise((resolve, reject) => {
      const checkInterval = setInterval(() => {
        const messages = this.messageLog.get(subject) || [];
        
        for (const msg of messages) {
          if (this.matchesCriteria(msg.data, criteria)) {
            clearInterval(checkInterval);
            resolve(msg.data);
            return;
          }
        }

        if (Date.now() - startTime > timeout) {
          clearInterval(checkInterval);
          reject(new Error(`Timeout waiting for message on ${subject} with criteria: ${JSON.stringify(criteria)}`));
        }
      }, 100);
    });
  }

  /**
   * Check if message matches criteria
   */
  private matchesCriteria(data: any, criteria: any): boolean {
    if (criteria.trace_id && data.trace_id !== criteria.trace_id) {
      return false;
    }
    if (criteria.assignment_id && data.assignment_id !== criteria.assignment_id) {
      return false;
    }
    return true;
  }

  /**
   * Simulate NATS disconnection for resilience testing
   */
  async simulateDisconnection(): Promise<void> {
    if (this.nc) {
      await this.nc.close();
      this.nc = null;
    }
  }

  /**
   * Reconnect after simulated disconnection
   */
  async reconnect(): Promise<void> {
    await this.connect();
  }

  /**
   * Get message history for subject
   */
  getMessageHistory(subject: string): any[] {
    return this.messageLog.get(subject) || [];
  }

  /**
   * Clear message history
   */
  clearMessageHistory(subject?: string): void {
    if (subject) {
      this.messageLog.delete(subject);
    } else {
      this.messageLog.clear();
    }
  }
}
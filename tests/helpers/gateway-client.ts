import axios, { AxiosInstance, AxiosResponse } from 'axios';
import { v4 as uuidv4 } from 'uuid';

/**
 * Gateway Client for Integration Tests
 * Provides HTTP client for testing Gateway API endpoints
 */
export class GatewayClient {
  private client: AxiosInstance;
  private resultSubscriptions: Map<string, (result: any) => void> = new Map();

  constructor(private config: {
    baseUrl: string;
    apiKey: string;
    timeout?: number;
  }) {
    this.client = axios.create({
      baseURL: config.baseUrl,
      timeout: config.timeout || 30000,
      headers: {
        'Authorization': `Bearer ${config.apiKey}`,
        'Content-Type': 'application/json'
      }
    });
  }

  /**
   * Send chat completion request
   */
  async chatCompletion(params: {
    messages: Array<{ role: string; content: string }>;
    model?: string;
    temperature?: number;
    max_tokens?: number;
    traceId?: string;
    tenantId?: string;
    policyId?: string;
  }): Promise<AxiosResponse> {
    const requestId = uuidv4();
    
    try {
      const response = await this.client.post('/api/v1/chat/completions', {
        messages: params.messages,
        model: params.model || 'gpt-4',
        temperature: params.temperature || 0.7,
        max_tokens: params.max_tokens || 1000,
        stream: false
      }, {
        headers: {
          'X-Request-ID': requestId,
          'X-Trace-ID': params.traceId || uuidv4(),
          'X-Tenant-ID': params.tenantId || 'default',
          'X-Policy-ID': params.policyId
        }
      });

      return response;
    } catch (error: any) {
      console.error('Gateway chat completion error:', error.response?.data || error.message);
      throw error;
    }
  }

  /**
   * Send completion request
   */
  async completion(params: {
    prompt: string;
    model?: string;
    temperature?: number;
    max_tokens?: number;
    traceId?: string;
    tenantId?: string;
    policyId?: string;
  }): Promise<AxiosResponse> {
    const requestId = uuidv4();
    
    try {
      const response = await this.client.post('/api/v1/completions', {
        prompt: params.prompt,
        model: params.model || 'gpt-4',
        temperature: params.temperature || 0.7,
        max_tokens: params.max_tokens || 1000,
        stream: false
      }, {
        headers: {
          'X-Request-ID': requestId,
          'X-Trace-ID': params.traceId || uuidv4(),
          'X-Tenant-ID': params.tenantId || 'default',
          'X-Policy-ID': params.policyId
        }
      });

      return response;
    } catch (error: any) {
      console.error('Gateway completion error:', error.response?.data || error.message);
      throw error;
    }
  }

  /**
   * Send embedding request
   */
  async embedding(params: {
    input: string | string[];
    model?: string;
    traceId?: string;
    tenantId?: string;
    policyId?: string;
  }): Promise<AxiosResponse> {
    const requestId = uuidv4();
    
    try {
      const response = await this.client.post('/api/v1/embeddings', {
        input: params.input,
        model: params.model || 'text-embedding-ada-002'
      }, {
        headers: {
          'X-Request-ID': requestId,
          'X-Trace-ID': params.traceId || uuidv4(),
          'X-Tenant-ID': params.tenantId || 'default',
          'X-Policy-ID': params.policyId
        }
      });

      return response;
    } catch (error: any) {
      console.error('Gateway embedding error:', error.response?.data || error.message);
      throw error;
    }
  }

  /**
   * Wait for result using SSE or polling
   */
  async waitForResult(assignmentId: string, options?: {
    timeout?: number;
    pollInterval?: number;
  }): Promise<any> {
    const timeout = options?.timeout || 30000;
    const pollInterval = options?.pollInterval || 1000;
    const startTime = Date.now();

    return new Promise((resolve, reject) => {
      const poll = async () => {
        try {
          // Try to get result via SSE first
          const sseResult = await this.getResultViaSSE(assignmentId, timeout);
          if (sseResult) {
            resolve(sseResult);
            return;
          }

          // Fallback to polling
          const response = await this.client.get(`/api/v1/results/${assignmentId}`);
          
          if (response.data.status === 'completed') {
            resolve(response.data);
            return;
          }

          if (response.data.status === 'failed') {
            reject(new Error(`Request failed: ${response.data.error}`));
            return;
          }

          if (Date.now() - startTime > timeout) {
            reject(new Error(`Timeout waiting for result ${assignmentId}`));
            return;
          }

          setTimeout(poll, pollInterval);
        } catch (error: any) {
          if (error.response?.status === 404) {
            // Result not ready yet, continue polling
            if (Date.now() - startTime > timeout) {
              reject(new Error(`Timeout waiting for result ${assignmentId}`));
              return;
            }
            setTimeout(poll, pollInterval);
          } else {
            reject(error);
          }
        }
      };

      poll();
    });
  }

  /**
   * Try to get result via SSE
   */
  private async getResultViaSSE(assignmentId: string, timeout: number): Promise<any | null> {
    return new Promise((resolve) => {
      const eventSource = new EventSource(
        `${this.config.baseUrl}/api/v1/results/stream?assignment_id=${assignmentId}`,
        {
          headers: {
            'Authorization': `Bearer ${this.config.apiKey}`
          }
        }
      );

      const timeoutId = setTimeout(() => {
        eventSource.close();
        resolve(null);
      }, timeout);

      eventSource.onmessage = (event) => {
        try {
          const data = JSON.parse(event.data);
          if (data.assignment_id === assignmentId && data.status) {
            clearTimeout(timeoutId);
            eventSource.close();
            resolve(data);
          }
        } catch (error) {
          console.error('SSE parse error:', error);
        }
      };

      eventSource.onerror = () => {
        clearTimeout(timeoutId);
        eventSource.close();
        resolve(null);
      };
    });
  }

  /**
   * Health check
   */
  async healthCheck(): Promise<AxiosResponse> {
    return this.client.get('/health');
  }

  /**
   * Get API info
   */
  async getApiInfo(): Promise<AxiosResponse> {
    return this.client.get('/api/v1/info');
  }
}
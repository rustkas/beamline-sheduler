# Worker 4: OpenAI Adapter Service - Detailed Technical Specification

## 🎯 Mission Overview

**Worker 4** (Frontend Developer) is responsible for creating the **OpenAI Adapter Service** that ensures perfect compatibility between OpenAI API format and BeamLine Router internal format. This service is critical for developer experience and platform adoption.

## 📋 Priority 2 Tasks Breakdown

### 1. OpenAI ↔ Router Format Conversion

#### **Core Conversion Logic**

```typescript
// apps/gateway/src/openai/openai-adapter.service.ts

import { Injectable, Logger } from '@nestjs/common';
import { ChatCompletionRequestDto, ChatCompletionResponseDto } from './dto/chat-completion.dto';
import { RouteRequestDto, RouteResponseDto } from '../router/dto/router.dto';

@Injectable()
export class OpenAIAdapterService {
  private readonly logger = new Logger(OpenAIAdapterService.name);

  /**
   * Convert OpenAI Chat Completion Request to Router Format
   * Handles all OpenAI-specific fields and maps to unified Router format
   */
  mapChatToRouterRequest(
    openaiRequest: ChatCompletionRequestDto,
    context: RequestContext
  ): RouteRequestDto {
    this.logger.debug(`Converting OpenAI request to Router format`, {
      model: openaiRequest.model,
      messageCount: openaiRequest.messages?.length,
      traceId: context.traceId
    });

    return {
      message: {
        id: this.generateMessageId(),
        tenant_id: context.tenantId,
        user_id: context.userId,
        content: this.extractContentFromMessages(openaiRequest.messages),
        metadata: {
          original_format: 'openai',
          model: openaiRequest.model,
          temperature: openaiRequest.temperature,
          max_tokens: openaiRequest.max_tokens,
          top_p: openaiRequest.top_p,
          frequency_penalty: openaiRequest.frequency_penalty,
          presence_penalty: openaiRequest.presence_penalty,
          stop: openaiRequest.stop,
          stream: openaiRequest.stream,
          user: openaiRequest.user,
          messages: openaiRequest.messages // Preserve original for context
        },
        timestamp: new Date().toISOString(),
        trace_id: context.traceId
      },
      policy: {
        provider_preferences: this.inferProviderFromModel(openaiRequest.model),
        timeout_ms: this.calculateTimeout(openaiRequest.max_tokens),
        retry_policy: {
          max_attempts: 3,
          backoff_strategy: 'exponential'
        },
        circuit_breaker: {
          failure_threshold: 5,
          recovery_timeout_ms: 30000
        }
      },
      context: {
        session_id: context.sessionId,
        conversation_history: this.buildConversationHistory(openaiRequest.messages),
        user_context: {
          openai_user: openaiRequest.user,
          original_request_id: context.requestId
        }
      }
    };
  }

  /**
   * Convert Router Response to OpenAI Chat Completion Format
   * Ensures 100% compatibility with OpenAI API specification
   */
  mapRouterToChatResponse(
    routerResponse: RouteResponseDto,
    originalRequest: ChatCompletionRequestDto
  ): ChatCompletionResponseDto {
    const choice = this.buildOpenAIChoice(routerResponse, originalRequest);
    const usage = this.buildOpenAIUsage(routerResponse);

    return {
      id: `chatcmpl-${routerResponse.message.id}`,
      object: 'chat.completion',
      created: Math.floor(Date.now() / 1000),
      model: originalRequest.model,
      choices: [choice],
      usage: usage,
      system_fingerprint: this.generateSystemFingerprint(routerResponse.decision.provider_id)
    };
  }

  /**
   * Extract content from OpenAI messages format
   * Handles both string and object message formats
   */
  private extractContentFromMessages(messages: any[]): string {
    if (!messages || messages.length === 0) {
      throw new Error('No messages provided in OpenAI format');
    }

    // Get the last user message
    const lastUserMessage = messages.reverse().find(msg => msg.role === 'user');
    
    if (!lastUserMessage) {
      throw new Error('No user message found in OpenAI format');
    }

    // Handle both string content and array content (for multimodal)
    if (typeof lastUserMessage.content === 'string') {
      return lastUserMessage.content;
    }

    if (Array.isArray(lastUserMessage.content)) {
      return lastUserMessage.content
        .filter(item => item.type === 'text')
        .map(item => item.text)
        .join(' ');
    }

    return JSON.stringify(lastUserMessage.content);
  }

  /**
   * Infer provider preferences from OpenAI model name
   * Maps OpenAI models to compatible providers
   */
  private inferProviderFromModel(model: string): string[] {
    const modelMapping = {
      'gpt-4': ['openai', 'azure-openai', 'anthropic-claude-3'],
      'gpt-4-turbo': ['openai', 'azure-openai'],
      'gpt-3.5-turbo': ['openai', 'azure-openai', 'anthropic-claude-instant'],
      'text-davinci-003': ['openai', 'azure-openai'],
      'text-embedding-ada-002': ['openai', 'azure-openai', 'cohere'],
      'dall-e-3': ['openai', 'azure-openai'],
      'whisper-1': ['openai', 'azure-openai']
    };

    const providers = modelMapping[model] || ['openai'];
    this.logger.debug(`Mapped model ${model} to providers:`, providers);
    return providers;
  }

  /**
   * Build OpenAI choice from Router response
   * Ensures proper OpenAI message format
   */
  private buildOpenAIChoice(
    routerResponse: RouteResponseDto,
    originalRequest: ChatCompletionRequestDto
  ): any {
    const content = routerResponse.message.content;
    const finishReason = this.mapFinishReason(routerResponse.metadata?.finish_reason);

    return {
      index: 0,
      message: {
        role: 'assistant',
        content: content,
        // Handle function calling if present
        tool_calls: routerResponse.metadata?.tool_calls,
        function_call: routerResponse.metadata?.function_call
      },
      finish_reason: finishReason,
      logprobs: this.buildLogprobs(routerResponse.metadata?.logprobs)
    };
  }

  /**
   * Map Router finish reason to OpenAI format
   */
  private mapFinishReason(routerFinishReason?: string): string {
    const finishReasonMapping = {
      'stop': 'stop',
      'length': 'length',
      'tool_calls': 'tool_calls',
      'function_call': 'function_call',
      'content_filter': 'content_filter'
    };

    return finishReasonMapping[routerFinishReason] || 'stop';
  }

  /**
   * Build OpenAI usage statistics
   */
  private buildOpenAIUsage(routerResponse: RouteResponseDto): any {
    const costEstimate = routerResponse.decision.cost_estimate;
    
    return {
      prompt_tokens: costEstimate?.input_tokens || 0,
      completion_tokens: costEstimate?.output_tokens || 0,
      total_tokens: (costEstimate?.input_tokens || 0) + (costEstimate?.output_tokens || 0)
    };
  }

  /**
   * Generate unique message ID
   */
  private generateMessageId(): string {
    return `msg_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  }

  /**
   * Calculate timeout based on max tokens
   */
  private calculateTimeout(maxTokens?: number): number {
    // Base timeout: 30 seconds + 50ms per token
    const baseTimeout = 30000;
    const tokenTimeout = (maxTokens || 2048) * 50;
    return Math.min(baseTimeout + tokenTimeout, 120000); // Max 2 minutes
  }

  /**
   * Build conversation history from messages
   */
  private buildConversationHistory(messages: any[]): any[] {
    return messages.map(msg => ({
      role: msg.role,
      content: typeof msg.content === 'string' ? msg.content : JSON.stringify(msg.content)
    }));
  }

  /**
   * Generate system fingerprint
   */
  private generateSystemFingerprint(providerId: string): string {
    return `beamline_${providerId}_${process.env.BUILD_VERSION || 'dev'}`;
  }

  /**
   * Build logprobs if available
   */
  private buildLogprobs(routerLogprobs?: any): any {
    if (!routerLogprobs) return null;
    
    return {
      content: routerLogprobs.tokens?.map((token, index) => ({
        token: token,
        logprob: routerLogprobs.token_logprobs?.[index] || 0,
        bytes: Array.from(token).map(c => c.charCodeAt(0)),
        top_logprobs: routerLogprobs.top_logprobs?.[index] || []
      })) || []
    };
  }
}
```

### 2. SSE Streaming Chunk Handling

#### **Real-time Streaming Implementation**

```typescript
// apps/gateway/src/openai/openai-adapter.service.ts

import { Observable } from 'rxjs';
import { map, catchError } from 'rxjs/operators';

@Injectable()
export class OpenAIAdapterService {

  /**
   * Convert Router SSE stream to OpenAI SSE format
   * Handles real-time chunk processing and formatting
   */
  mapRouterStreamToOpenAI(
    routerStream: Observable<RouteResponseDto>,
    originalRequest: ChatCompletionRequestDto
  ): Observable<string> {
    let chunkIndex = 0;
    let accumulatedContent = '';
    
    return routerStream.pipe(
      map((routerChunk: RouteResponseDto) => {
        chunkIndex++;
        
        // Accumulate content for delta calculation
        const newContent = routerChunk.message.content;
        const contentDelta = newContent.substring(accumulatedContent.length);
        accumulatedContent = newContent;
        
        // Build OpenAI SSE chunk
        const openAIChunk = this.buildOpenAISSEChunk({
          routerChunk,
          originalRequest,
          chunkIndex,
          contentDelta,
          isFinal: routerChunk.metadata?.finish_reason !== undefined
        });
        
        this.logger.debug(`Converted Router stream chunk to OpenAI SSE`, {
          chunkIndex,
          contentLength: contentDelta.length,
          isFinal: openAIChunk.finish_reason !== null
        });
        
        return `data: ${JSON.stringify(openAIChunk)}\n\n`;
      }),
      catchError((error) => {
        this.logger.error('Error in stream conversion', error);
        const errorChunk = this.buildOpenAIErrorChunk(error);
        return `data: ${JSON.stringify(errorChunk)}\n\n`;
      })
    );
  }

  /**
   * Build individual OpenAI SSE chunk
   */
  private buildOpenAISSEChunk(params: {
    routerChunk: RouteResponseDto;
    originalRequest: ChatCompletionRequestDto;
    chunkIndex: number;
    contentDelta: string;
    isFinal: boolean;
  }): any {
    const { routerChunk, originalRequest, chunkIndex, contentDelta, isFinal } = params;
    
    const chunkId = `chatcmpl-${routerChunk.message.id}-${chunkIndex}`;
    const timestamp = Math.floor(Date.now() / 1000);
    
    return {
      id: chunkId,
      object: 'chat.completion.chunk',
      created: timestamp,
      model: originalRequest.model,
      choices: [{
        index: 0,
        delta: this.buildDeltaObject(contentDelta, routerChunk, isFinal),
        finish_reason: isFinal ? this.mapFinishReason(routerChunk.metadata?.finish_reason) : null,
        logprobs: this.buildStreamingLogprobs(routerChunk.metadata?.logprobs)
      }],
      system_fingerprint: this.generateSystemFingerprint(routerChunk.decision.provider_id)
    };
  }

  /**
   * Build delta object for streaming
   */
  private buildDeltaObject(
    contentDelta: string,
    routerChunk: RouteResponseDto,
    isFinal: boolean
  ): any {
    const delta: any = {};
    
    if (contentDelta && contentDelta.length > 0) {
      delta.content = contentDelta;
    }
    
    // Handle tool calls in streaming
    if (routerChunk.metadata?.tool_calls) {
      delta.tool_calls = routerChunk.metadata.tool_calls.map((call, index) => ({
        index: index,
        id: call.id,
        type: call.type,
        function: {
          name: call.function?.name,
          arguments: call.function?.arguments
        }
      }));
    }
    
    // Handle function calls (legacy)
    if (routerChunk.metadata?.function_call) {
      delta.function_call = routerChunk.metadata.function_call;
    }
    
    return delta;
  }

  /**
   * Build streaming logprobs
   */
  private buildStreamingLogprobs(routerLogprobs?: any): any {
    if (!routerLogprobs) return null;
    
    return {
      content: routerLogprobs.tokens?.map((token, index) => ({
        token: token,
        logprob: routerLogprobs.token_logprobs?.[index] || 0,
        bytes: Array.from(token).map(c => c.charCodeAt(0))
      })) || []
    };
  }

  /**
   * Build error chunk for SSE stream
   */
  private buildOpenAIErrorChunk(error: any): any {
    return {
      error: {
        message: error.message || 'Stream processing error',
        type: 'stream_error',
        code: 'internal_error',
        param: error.details || {}
      }
    };
  }

  /**
   * Handle stream completion
   */
  buildStreamCompletionSignal(): string {
    return 'data: [DONE]\n\n';
  }

  /**
   * Handle stream keepalive
   */
  buildStreamKeepalive(): string {
    return ': keepalive\n\n';
  }
}
```

### 3. Error Compatibility Mapping

#### **Comprehensive Error Translation**

```typescript
// apps/gateway/src/openai/openai-adapter.service.ts

interface OpenAIErrorResponse {
  error: {
    message: string;
    type: string;
    code: string;
    param?: any;
    trace_id?: string;
  }
}

@Injectable()
export class OpenAIAdapterService {

  /**
   * Map Router errors to OpenAI-compatible format
   * Ensures 100% compatibility with OpenAI error handling
   */
  mapRouterErrorToOpenAI(routerError: any): OpenAIErrorResponse {
    const errorCode = routerError.code || 'SYSTEM_INTERNAL_ERROR';
    const errorMapping = this.getOpenAIErrorMapping(errorCode);
    
    this.logger.warn(`Mapping Router error to OpenAI format`, {
      originalCode: errorCode,
      mappedCode: errorMapping.code,
      type: errorMapping.type
    });

    return {
      error: {
        message: this.buildErrorMessage(routerError, errorMapping),
        type: errorMapping.type,
        code: errorMapping.code,
        param: routerError.details || routerError.param,
        trace_id: routerError.trace_id
      }
    };
  }

  /**
   * Get OpenAI error mapping for Router error codes
   */
  private getOpenAIErrorMapping(routerErrorCode: string): {
    type: string;
    code: string;
    statusCode: number;
  } {
    const errorMappings = {
      // Authentication errors
      'AUTH_INVALID_TOKEN': {
        type: 'invalid_request_error',
        code: 'invalid_api_key',
        statusCode: 401
      },
      'AUTH_INSUFFICIENT_PERMISSIONS': {
        type: 'invalid_request_error',
        code: 'insufficient_permissions',
        statusCode: 403
      },
      'AUTH_QUOTA_EXCEEDED': {
        type: 'invalid_request_error',
        code: 'quota_exceeded',
        statusCode: 429
      },
      'AUTH_RATE_LIMIT_EXCEEDED': {
        type: 'rate_limit_error',
        code: 'rate_limit_exceeded',
        statusCode: 429
      },

      // Routing errors
      'ROUTE_NO_PROVIDER_AVAILABLE': {
        type: 'api_error',
        code: 'service_unavailable',
        statusCode: 503
      },
      'ROUTE_POLICY_VIOLATION': {
        type: 'invalid_request_error',
        code: 'policy_violation',
        statusCode: 400
      },
      'ROUTE_TIMEOUT': {
        type: 'api_error',
        code: 'request_timeout',
        statusCode: 504
      },
      'ROUTE_CIRCUIT_BREAKER_OPEN': {
        type: 'api_error',
        code: 'service_unavailable',
        statusCode: 503
      },

      // Provider errors
      'PROVIDER_UNAVAILABLE': {
        type: 'api_error',
        code: 'service_unavailable',
        statusCode: 503
      },
      'PROVIDER_RATE_LIMITED': {
        type: 'rate_limit_error',
        code: 'rate_limit_exceeded',
        statusCode: 429
      },
      'PROVIDER_INSUFFICIENT_FUNDS': {
        type: 'invalid_request_error',
        code: 'billing_error',
        statusCode: 402
      },
      'PROVIDER_MODEL_NOT_FOUND': {
        type: 'invalid_request_error',
        code: 'model_not_found',
        statusCode: 404
      },

      // Content filtering
      'CONTENT_POLICY_VIOLATION': {
        type: 'invalid_request_error',
        code: 'content_policy_violation',
        statusCode: 400
      },

      // System errors
      'SYSTEM_INTERNAL_ERROR': {
        type: 'api_error',
        code: 'internal_error',
        statusCode: 500
      },
      'SYSTEM_SERVICE_UNAVAILABLE': {
        type: 'api_error',
        code: 'service_unavailable',
        statusCode: 503
      },
      'SYSTEM_MAINTENANCE': {
        type: 'api_error',
        code: 'service_unavailable',
        statusCode: 503
      }
    };

    return errorMappings[routerErrorCode] || {
      type: 'api_error',
      code: 'internal_error',
      statusCode: 500
    };
  }

  /**
   * Build user-friendly error message
   */
  private buildErrorMessage(routerError: any, errorMapping: any): string {
    const originalMessage = routerError.message || 'An error occurred';
    
    // Add context-specific guidance
    const contextMessages = {
      'invalid_api_key': 'Your API key is invalid. Please check your API key and try again.',
      'insufficient_permissions': 'Your API key does not have permission to perform this action.',
      'quota_exceeded': 'You have exceeded your quota. Please check your usage and billing details.',
      'rate_limit_exceeded': 'Rate limit exceeded. Please slow down your requests.',
      'service_unavailable': 'The service is temporarily unavailable. Please try again later.',
      'model_not_found': 'The specified model does not exist or you do not have access to it.',
      'content_policy_violation': 'Your request was rejected due to content policy violations.',
      'billing_error': 'There is an issue with your billing. Please check your payment method.'
    };

    const userMessage = contextMessages[errorMapping.code] || originalMessage;
    
    return `${userMessage} (Trace ID: ${routerError.trace_id || 'unknown'})`;
  }

  /**
   * Handle streaming-specific errors
   */
  mapStreamErrorToOpenAI(error: any): string {
    const openAIError = this.mapRouterErrorToOpenAI(error);
    return `data: ${JSON.stringify(openAIError)}\n\n`;
  }

  /**
   * Get HTTP status code for Router error
   */
  getHttpStatusForRouterError(routerError: any): number {
    const errorMapping = this.getOpenAIErrorMapping(routerError.code || 'SYSTEM_INTERNAL_ERROR');
    return errorMapping.statusCode;
  }

  /**
   * Validate OpenAI request format
   */
  validateOpenAIRequest(request: any): { valid: boolean; errors: string[] } {
    const errors: string[] = [];

    // Required fields validation
    if (!request.model) {
      errors.push('model is required');
    }

    if (!request.messages || !Array.isArray(request.messages)) {
      errors.push('messages must be an array');
    } else if (request.messages.length === 0) {
      errors.push('messages array cannot be empty');
    }

    // Validate message format
    request.messages?.forEach((msg, index) => {
      if (!msg.role) {
        errors.push(`message[${index}].role is required`);
      }
      
      if (!msg.content && !msg.tool_calls && !msg.function_call) {
        errors.push(`message[${index}] must have content, tool_calls, or function_call`);
      }

      // Validate role values
      const validRoles = ['system', 'user', 'assistant', 'tool'];
      if (msg.role && !validRoles.includes(msg.role)) {
        errors.push(`message[${index}].role must be one of: ${validRoles.join(', ')}`);
      }
    });

    // Parameter validation
    if (request.temperature !== undefined) {
      if (typeof request.temperature !== 'number' || request.temperature < 0 || request.temperature > 2) {
        errors.push('temperature must be a number between 0 and 2');
      }
    }

    if (request.top_p !== undefined) {
      if (typeof request.top_p !== 'number' || request.top_p < 0 || request.top_p > 1) {
        errors.push('top_p must be a number between 0 and 1');
      }
    }

    if (request.max_tokens !== undefined) {
      if (typeof request.max_tokens !== 'number' || request.max_tokens < 1) {
        errors.push('max_tokens must be a positive integer');
      }
    }

    if (request.n !== undefined) {
      if (typeof request.n !== 'number' || request.n < 1 || request.n > 10) {
        errors.push('n must be an integer between 1 and 10');
      }
    }

    return {
      valid: errors.length === 0,
      errors
    };
  }
}
```

### 4. Swagger Documentation

#### **Complete API Documentation**

```typescript
// apps/gateway/src/openai/dto/openai-swagger.dto.ts

import { ApiProperty, ApiPropertyOptional } from '@nestjs/swagger';

export class ChatCompletionMessageDto {
  @ApiProperty({
    description: 'The role of the message author',
    enum: ['system', 'user', 'assistant', 'tool'],
    example: 'user'
  })
  role: string;

  @ApiProperty({
    description: 'The contents of the message',
    example: 'Hello, how are you?'
  })
  content: string;

  @ApiPropertyOptional({
    description: 'The name of the author of this message',
    example: 'john_doe'
  })
  name?: string;

  @ApiPropertyOptional({
    description: 'The tool calls generated by the model',
    type: 'array',
    items: {
      type: 'object',
      properties: {
        id: { type: 'string', example: 'call_123' },
        type: { type: 'string', example: 'function' },
        function: {
          type: 'object',
          properties: {
            name: { type: 'string', example: 'get_weather' },
            arguments: { type: 'string', example: '{"location": "San Francisco"}' }
          }
        }
      }
    }
  })
  tool_calls?: any[];

  @ApiPropertyOptional({
    description: 'The tool call that this message is responding to',
    example: 'call_123'
  })
  tool_call_id?: string;
}

export class ChatCompletionRequestDto {
  @ApiProperty({
    description: 'ID of the model to use',
    example: 'gpt-4',
    required: true
  })
  model: string;

  @ApiProperty({
    description: 'A list of messages comprising the conversation so far',
    type: [ChatCompletionMessageDto],
    required: true
  })
  messages: ChatCompletionMessageDto[];

  @ApiPropertyOptional({
    description: 'Number of completions to generate',
    example: 1,
    minimum: 1,
    maximum: 10,
    default: 1
  })
  n?: number;

  @ApiPropertyOptional({
    description: 'What sampling temperature to use',
    example: 0.7,
    minimum: 0,
    maximum: 2,
    default: 1
  })
  temperature?: number;

  @ApiPropertyOptional({
    description: 'An alternative to sampling with temperature',
    example: 1,
    minimum: 0,
    maximum: 1,
    default: 1
  })
  top_p?: number;

  @ApiPropertyOptional({
    description: 'Maximum number of tokens to generate',
    example: 2048,
    minimum: 1
  })
  max_tokens?: number;

  @ApiPropertyOptional({
    description: 'Whether to stream back partial progress',
    example: true,
    default: false
  })
  stream?: boolean;

  @ApiPropertyOptional({
    description: 'Up to 4 sequences where the API will stop generating',
    example: ['\n', 'Human:'],
    type: [String]
  })
  stop?: string | string[];

  @ApiPropertyOptional({
    description: 'Presence penalty parameter',
    example: 0,
    minimum: -2,
    maximum: 2,
    default: 0
  })
  presence_penalty?: number;

  @ApiPropertyOptional({
    description: 'Frequency penalty parameter',
    example: 0,
    minimum: -2,
    maximum: 2,
    default: 0
  })
  frequency_penalty?: number;

  @ApiPropertyOptional({
    description: 'Modify the likelihood of specified tokens',
    example: { '50256': -100 }
  })
  logit_bias?: Record<string, number>;

  @ApiPropertyOptional({
    description: 'A unique identifier representing your end-user',
    example: 'user_123'
  })
  user?: string;

  @ApiPropertyOptional({
    description: 'Return log probabilities of the output tokens',
    example: true,
    default: false
  })
  logprobs?: boolean;

  @ApiPropertyOptional({
    description: 'The maximum number of logprobs to return',
    example: 5,
    minimum: 0,
    maximum: 20
  })
  top_logprobs?: number;

  @ApiPropertyOptional({
    description: 'A list of functions the model may generate JSON inputs for',
    type: 'array',
    items: {
      type: 'object',
      properties: {
        name: { type: 'string', description: 'The name of the function' },
        description: { type: 'string', description: 'A description of what the function does' },
        parameters: { type: 'object', description: 'The parameters the functions accepts' }
      }
    }
  })
  functions?: any[];

  @ApiPropertyOptional({
    description: 'Controls how the model responds to function calls',
    example: 'auto',
    enum: ['none', 'auto', 'specific_function']
  })
  function_call?: string | any;

  @ApiPropertyOptional({
    description: 'A list of tools the model may call',
    type: 'array',
    items: {
      type: 'object',
      properties: {
        type: { type: 'string', example: 'function' },
        function: {
          type: 'object',
          properties: {
            name: { type: 'string', description: 'The name of the function' },
            description: { type: 'string', description: 'A description of what the function does' },
            parameters: { type: 'object', description: 'The parameters the functions accepts' }
          }
        }
      }
    }
  })
  tools?: any[];

  @ApiPropertyOptional({
    description: 'Controls which tool the model should call',
    example: 'auto',
    enum: ['none', 'auto', 'required', 'specific_tool']
  })
  tool_choice?: string | any;

  @ApiPropertyOptional({
    description: 'Whether to return the response in a specific format',
    example: { type: 'json_object' },
    type: 'object'
  })
  response_format?: any;

  @ApiPropertyOptional({
    description: 'The seed to use for generation',
    example: 42,
    minimum: 0
  })
  seed?: number;
}

export class ChatCompletionResponseDto {
  @ApiProperty({
    description: 'A unique identifier for the completion',
    example: 'chatcmpl-123'
  })
  id: string;

  @ApiProperty({
    description: 'The object type, which is always chat.completion',
    example: 'chat.completion'
  })
  object: string;

  @ApiProperty({
    description: 'The Unix timestamp when the completion was created',
    example: 1677652288
  })
  created: number;

  @ApiProperty({
    description: 'The model used for completion',
    example: 'gpt-4'
  })
  model: string;

  @ApiProperty({
    description: 'The list of completion choices',
    type: 'array',
    items: {
      type: 'object',
      properties: {
        index: { type: 'number', example: 0 },
        message: { $ref: '#/components/schemas/ChatCompletionMessageDto' },
        finish_reason: { type: 'string', example: 'stop' },
        logprobs: { type: 'object' }
      }
    }
  })
  choices: any[];

  @ApiProperty({
    description: 'Usage statistics for the completion request',
    type: 'object',
    properties: {
      prompt_tokens: { type: 'number', example: 10 },
      completion_tokens: { type: 'number', example: 20 },
      total_tokens: { type: 'number', example: 30 }
    }
  })
  usage: any;

  @ApiPropertyOptional({
    description: 'System fingerprint for the model configuration',
    example: 'fp_44709d2abc'
  })
  system_fingerprint?: string;
}
```

#### **Controller Swagger Documentation**

```typescript
// apps/gateway/src/openai/openai.controller.ts

import { Controller, Post, Body, Headers, Res, HttpStatus } from '@nestjs/common';
import { ApiTags, ApiOperation, ApiResponse, ApiHeader, ApiBody } from '@nestjs/swagger';
import { Response } from 'express';

@ApiTags('OpenAI Compatible API')
@Controller('v1')
export class OpenAIController {

  @Post('chat/completions')
  @ApiOperation({
    summary: 'Create chat completion',
    description: 'Creates a completion for the chat conversation. Compatible with OpenAI API specification.'
  })
  @ApiHeader({
    name: 'Authorization',
    description: 'Bearer token for authentication',
    required: true,
    example: 'Bearer sk-ant-api03-...'
  })
  @ApiHeader({
    name: 'Content-Type',
    description: 'Must be application/json',
    required: true,
    example: 'application/json'
  })
  @ApiBody({
    type: ChatCompletionRequestDto,
    description: 'Chat completion request parameters',
    examples: {
      basic: {
        summary: 'Basic chat completion',
        value: {
          model: 'gpt-4',
          messages: [
            { role: 'user', content: 'Hello, how are you?' }
          ]
        }
      },
      streaming: {
        summary: 'Streaming chat completion',
        value: {
          model: 'gpt-4',
          messages: [
            { role: 'user', content: 'Tell me a story' }
          ],
          stream: true
        }
      },
      with_tools: {
        summary: 'Chat completion with tools',
        value: {
          model: 'gpt-4',
          messages: [
            { role: 'user', content: 'What\'s the weather in San Francisco?' }
          ],
          tools: [
            {
              type: 'function',
              function: {
                name: 'get_weather',
                description: 'Get weather information for a location',
                parameters: {
                  type: 'object',
                  properties: {
                    location: { type: 'string' }
                  },
                  required: ['location']
                }
              }
            }
          ]
        }
      }
    }
  })
  @ApiResponse({
    status: 200,
    description: 'Successful chat completion',
    type: ChatCompletionResponseDto,
    content: {
      'application/json': {
        example: {
          id: 'chatcmpl-123',
          object: 'chat.completion',
          created: 1677652288,
          model: 'gpt-4',
          choices: [{
            index: 0,
            message: {
              role: 'assistant',
              content: 'I\'m doing well, thank you for asking! How can I help you today?'
            },
            finish_reason: 'stop'
          }],
          usage: {
            prompt_tokens: 10,
            completion_tokens: 20,
            total_tokens: 30
          },
          system_fingerprint: 'beamline_openai_v1.0'
        }
      }
    }
  })
  @ApiResponse({
    status: 400,
    description: 'Bad request - invalid parameters',
    content: {
      'application/json': {
        example: {
          error: {
            message: 'model is required',
            type: 'invalid_request_error',
            code: 'missing_parameter',
            param: 'model'
          }
        }
      }
    }
  })
  @ApiResponse({
    status: 401,
    description: 'Unauthorized - invalid API key',
    content: {
      'application/json': {
        example: {
          error: {
            message: 'Invalid API key provided',
            type: 'invalid_request_error',
            code: 'invalid_api_key'
          }
        }
      }
    }
  })
  @ApiResponse({
    status: 429,
    description: 'Rate limit exceeded',
    content: {
      'application/json': {
        example: {
          error: {
            message: 'Rate limit exceeded. Please slow down your requests.',
            type: 'rate_limit_error',
            code: 'rate_limit_exceeded'
          }
        }
      }
    }
  })
  @ApiResponse({
    status: 500,
    description: 'Internal server error',
    content: {
      'application/json': {
        example: {
          error: {
            message: 'An internal error occurred. Please try again later.',
            type: 'api_error',
            code: 'internal_error'
          }
        }
      }
    }
  })
  async chatCompletions(
    @Body() request: ChatCompletionRequestDto,
    @Headers('authorization') authorization: string,
    @Res() response: Response
  ) {
    // Implementation here
  }
}
```

## 🧪 Testing Requirements

### Unit Tests

```typescript
// apps/gateway/src/openai/tests/openai-adapter.service.spec.ts

describe('OpenAIAdapterService', () => {
  let service: OpenAIAdapterService;

  beforeEach(() => {
    service = new OpenAIAdapterService();
  });

  describe('mapChatToRouterRequest', () => {
    it('should convert basic OpenAI request to Router format', () => {
      const openaiRequest = {
        model: 'gpt-4',
        messages: [
          { role: 'user', content: 'Hello, how are you?' }
        ]
      };

      const context = {
        tenantId: 'tenant_123',
        userId: 'user_456',
        traceId: 'trace_789'
      };

      const result = service.mapChatToRouterRequest(openaiRequest, context);

      expect(result.message.content).toBe('Hello, how are you?');
      expect(result.message.tenant_id).toBe('tenant_123');
      expect(result.policy.provider_preferences).toContain('openai');
    });

    it('should handle multimodal content', () => {
      const openaiRequest = {
        model: 'gpt-4-vision-preview',
        messages: [
          {
            role: 'user',
            content: [
              { type: 'text', text: 'What\'s in this image?' },
              { type: 'image_url', image_url: { url: 'https://example.com/image.jpg' } }
            ]
          }
        ]
      };

      const result = service.mapChatToRouterRequest(openaiRequest, {} as any);
      
      expect(result.message.content).toContain('What\'s in this image?');
    });
  });

  describe('mapRouterToChatResponse', () => {
    it('should convert Router response to OpenAI format', () => {
      const routerResponse = {
        message: {
          id: 'msg_123',
          content: 'I am doing well, thank you!'
        },
        decision: {
          provider_id: 'openai',
          model_id: 'gpt-4',
          cost_estimate: {
            input_tokens: 10,
            output_tokens: 20,
            total_cost: 0.001
          }
        }
      };

      const originalRequest = {
        model: 'gpt-4',
        messages: []
      };

      const result = service.mapRouterToChatResponse(routerResponse, originalRequest);

      expect(result.object).toBe('chat.completion');
      expect(result.choices[0].message.content).toBe('I am doing well, thank you!');
      expect(result.usage.total_tokens).toBe(30);
    });
  });

  describe('Error Mapping', () => {
    it('should map authentication errors correctly', () => {
      const routerError = {
        code: 'AUTH_INVALID_TOKEN',
        message: 'Invalid token',
        trace_id: 'trace_123'
      };

      const result = service.mapRouterErrorToOpenAI(routerError);

      expect(result.error.type).toBe('invalid_request_error');
      expect(result.error.code).toBe('invalid_api_key');
      expect(result.error.message).toContain('Trace ID: trace_123');
    });

    it('should handle rate limit errors', () => {
      const routerError = {
        code: 'PROVIDER_RATE_LIMITED',
        message: 'Rate limit exceeded'
      };

      const result = service.mapRouterErrorToOpenAI(routerError);

      expect(result.error.type).toBe('rate_limit_error');
      expect(result.error.code).toBe('rate_limit_exceeded');
    });
  });

  describe('SSE Streaming', () => {
    it('should convert Router stream to OpenAI SSE format', (done) => {
      const routerStream = new Observable(subscriber => {
        subscriber.next({
          message: { content: 'Hello' },
          decision: { provider_id: 'openai' }
        });
        subscriber.next({
          message: { content: 'Hello there' },
          decision: { provider_id: 'openai' }
        });
        subscriber.complete();
      });

      const originalRequest = { model: 'gpt-4', messages: [] };
      const result = service.mapRouterStreamToOpenAI(routerStream, originalRequest);

      const chunks: string[] = [];
      result.subscribe({
        next: (chunk) => chunks.push(chunk),
        complete: () => {
          expect(chunks.length).toBe(2);
          expect(chunks[0]).toContain('data:');
          expect(chunks[0]).toContain('chat.completion.chunk');
          done();
        }
      });
    });
  });

  describe('Request Validation', () => {
    it('should validate required fields', () => {
      const invalidRequest = {
        messages: []
      };

      const result = service.validateOpenAIRequest(invalidRequest);

      expect(result.valid).toBe(false);
      expect(result.errors).toContain('model is required');
    });

    it('should validate message format', () => {
      const invalidRequest = {
        model: 'gpt-4',
        messages: [
          { role: 'invalid_role', content: 'Hello' }
        ]
      };

      const result = service.validateOpenAIRequest(invalidRequest);

      expect(result.valid).toBe(false);
      expect(result.errors.some(err => err.includes('role must be one of'))).toBe(true);
    });

    it('should validate temperature range', () => {
      const invalidRequest = {
        model: 'gpt-4',
        messages: [{ role: 'user', content: 'Hello' }],
        temperature: 3.0
      };

      const result = service.validateOpenAIRequest(invalidRequest);

      expect(result.valid).toBe(false);
      expect(result.errors).toContain('temperature must be a number between 0 and 2');
    });
  });
});
```

### Integration Tests

```typescript
// apps/gateway/src/openai/tests/openai-integration.spec.ts

describe('OpenAI Integration Tests', () => {
  let app: INestApplication;
  let openAIAdapterService: OpenAIAdapterService;

  beforeAll(async () => {
    const moduleRef = await Test.createTestingModule({
      imports: [OpenAIModule]
    }).compile();

    app = moduleRef.createNestApplication();
    await app.init();

    openAIAdapterService = moduleRef.get<OpenAIAdapterService>(OpenAIAdapterService);
  });

  describe('End-to-end OpenAI Compatibility', () => {
    it('should handle complete chat completion flow', async () => {
      const request = {
        model: 'gpt-4',
        messages: [
          { role: 'user', content: 'What is 2+2?' }
        ],
        temperature: 0.7,
        max_tokens: 100
      };

      // Mock Router response
      const mockRouterResponse = {
        message: {
          id: 'msg_test',
          content: '2+2 equals 4'
        },
        decision: {
          provider_id: 'openai',
          model_id: 'gpt-4',
          cost_estimate: {
            input_tokens: 5,
            output_tokens: 4,
            total_cost: 0.0001
          }
        }
      };

      jest.spyOn(openAIAdapterService, 'mapChatToRouterRequest').mockReturnValue({
        message: { id: 'test_msg', content: 'What is 2+2?' }
      } as any);

      jest.spyOn(openAIAdapterService, 'mapRouterToChatResponse').mockReturnValue({
        id: 'chatcmpl-test',
        object: 'chat.completion',
        created: Math.floor(Date.now() / 1000),
        model: 'gpt-4',
        choices: [{
          index: 0,
          message: {
            role: 'assistant',
            content: '2+2 equals 4'
          },
          finish_reason: 'stop'
        }],
        usage: {
          prompt_tokens: 5,
          completion_tokens: 4,
          total_tokens: 9
        }
      });

      const result = await request(app.getHttpServer())
        .post('/v1/chat/completions')
        .set('Authorization', 'Bearer test-key')
        .send(request)
        .expect(200);

      expect(result.body.object).toBe('chat.completion');
      expect(result.body.choices[0].message.content).toBe('2+2 equals 4');
      expect(result.body.usage.total_tokens).toBe(9);
    });

    it('should handle streaming responses', (done) => {
      const request = {
        model: 'gpt-4',
        messages: [
          { role: 'user', content: 'Count to 3' }
        ],
        stream: true
      };

      const chunks: string[] = [];
      
      request(app.getHttpServer())
        .post('/v1/chat/completions')
        .set('Authorization', 'Bearer test-key')
        .send(request)
        .expect(200)
        .end((err, res) => {
          if (err) return done(err);
          
          const lines = res.text.split('\n');
          const dataLines = lines.filter(line => line.startsWith('data: '));
          
          expect(dataLines.length).toBeGreaterThan(0);
          expect(dataLines[dataLines.length - 1]).toBe('data: [DONE]');
          
          done();
        });
    });
  });
});
```

## 📊 Performance Requirements

### Latency Targets
- **Request conversion**: < 5ms
- **Response conversion**: < 3ms
- **SSE chunk processing**: < 1ms per chunk
- **Error mapping**: < 2ms

### Memory Usage
- **Max memory per request**: 10MB
- **Stream buffer size**: 64KB
- **Error object size**: < 1KB

### Throughput
- **Requests per second**: 1000+
- **Concurrent streams**: 500+
- **Error handling**: 5000+ errors/second

## 🔧 Implementation Guidelines

### Code Quality Standards
1. **Type Safety**: Use strict TypeScript with comprehensive interfaces
2. **Error Handling**: Never throw raw errors, always map to OpenAI format
3. **Logging**: Include trace_id in all log messages
4. **Validation**: Validate all inputs before processing
5. **Testing**: 100% unit test coverage for critical paths

### Security Considerations
1. **Input Sanitization**: Sanitize all user inputs
2. **Rate Limiting**: Implement per-user rate limits
3. **Authentication**: Validate API keys before processing
4. **Content Filtering**: Apply content policies consistently
5. **Audit Logging**: Log all requests with user context

### Performance Optimization
1. **Caching**: Cache provider mappings and validation results
2. **Streaming**: Use backpressure for large streams
3. **Batching**: Batch multiple small requests when possible
4. **Connection Pooling**: Reuse HTTP connections
5. **Memory Management**: Clean up streams and buffers promptly

This comprehensive specification provides Worker 4 with everything needed to implement a production-ready OpenAI Adapter Service that ensures perfect compatibility and excellent developer experience.
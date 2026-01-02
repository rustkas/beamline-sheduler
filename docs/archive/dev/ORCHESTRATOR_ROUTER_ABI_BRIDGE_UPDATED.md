---
version: 2.0
authors:
  - WORKER wrk-2: Architecture/Tech Lead
last_update: 2025-11-16T12:00:00Z
status: implementation-ready
rule_version: v10
message_protocol: v1
---

# Orchestrator/Router ABI Bridge Design - Implementation Plan

## Purpose

This document provides a concrete implementation plan for migrating new ABI fields from the current Router ABI (`beamline.flow.v1`) to the future Orchestrator/Worker ABI (`beamline.orchestrator.v1`) from BeamLine Vision, with detailed phases and specific code modifications.

## Current State: Router ABI (beamline.flow.v1)

### Current Message Structure

**`Message`** (proto/beamline/flow/v1/flow.proto):
```protobuf
message Message {
  string message_id = 1;        // ✅ Present
  string tenant_id = 2;         // ✅ Present
  string trace_id = 3;          // ✅ Present (optional)
  string message_type = 4;      // ✅ Present
  bytes payload = 5;            // ✅ Present
  map<string, string> metadata = 6;  // ✅ Present (optional)
  int64 timestamp_ms = 7;       // ✅ Present (optional)
}
```

**`RouteRequest`**:
```protobuf
message RouteRequest {
  Message message = 1;           // ✅ Present
  string policy_id = 2;         // ✅ Present (optional)
  map<string, string> context = 3;  // ✅ Present (optional)
}
```

## Implementation Phases

### Phase 1: Router Enhancement (CP2) - Optional Field Reading

**Goal**: Router starts reading `run_id`, `flow_id`, `step_id`, `idempotency_key` if present but doesn't require them.

**Timeline**: Week 1 of CP2
**Assigned to**: wrk-3 (Router/Erlang)

#### Required Router Module Modifications

**File**: `apps/otp/router/src/router_nats_subscriber.erl`

**Changes**:
```erlang
%% Add to handle_info/2 function around line 45-50
handle_info({nats_msg, Subject, Payload}, State) ->
    try
        Message = beamline_flow_v1_pb:decode_msg(Payload, 'Message'),
        
        %% NEW: Extract optional Orchestrator fields
        RunId = case maps:get(run_id, Message, undefined) of
            undefined -> maps:get(message_id, Message);  % Fallback to message_id
            Value -> Value
        end,
        
        FlowId = maps:get(flow_id, Message, undefined),
        StepId = case maps:get(step_id, Message, undefined) of
            undefined -> maps:get(message_id, Message);  % Fallback to message_id
            Value -> Value
        end,
        
        IdempotencyKey = case maps:get(idempotency_key, Message, undefined) of
            undefined -> maps:get(message_id, Message);  % Fallback to message_id
            Value -> Value
        end,
        
        %% NEW: Enhanced trace context
        TraceContext = #{
            trace_id => maps:get(trace_id, Message, undefined),
            span_id => maps:get(span_id, Message, undefined),
            run_id => RunId,
            flow_id => FlowId,
            step_id => StepId
        },
        
        %% Continue with existing processing
        process_message(Message, TraceContext, State)
    catch
        Error:Reason ->
            logger:error("Failed to decode NATS message: ~p:~p", [Error, Reason]),
            {noreply, State}
    end;
```

**File**: `apps/otp/router/src/router_idempotency.erl`

**Changes**:
```erlang
%% Modify check_idempotency/2 function around line 25-30
check_idempotency(Message, State) ->
    %% NEW: Use idempotency_key if present, fallback to message_id
    IdempotencyKey = case maps:get(idempotency_key, Message, undefined) of
        undefined -> maps:get(message_id, Message);
        Value -> Value
    end,
    
    %% Existing idempotency logic
    case ets:lookup(idempotency_table, IdempotencyKey) of
        [] -> 
            {not_processed, IdempotencyKey};
        [{IdempotencyKey, Result}] -> 
            {processed, Result};
        _ -> 
            {error, duplicate_key}
    end.
```

**File**: `apps/otp/router/src/router_trace.erl`

**Changes**:
```erlang
%% Add new function to handle enhanced trace context
start_trace_span(TraceContext, Message) ->
    %% NEW: Enhanced trace span with Orchestrator fields
    SpanName = case maps:get(step_id, TraceContext, undefined) of
        undefined -> "router.process";
        StepId -> io_lib:format("router.process.~s", [StepId])
    end,
    
    Attributes = [
        {<<"beamline.tenant_id">>, maps:get(tenant_id, Message)},
        {<<"beamline.message_type">>, maps:get(message_type, Message)},
        %% NEW: Add Orchestrator fields as trace attributes
        {<<"beamline.run_id">>, maps:get(run_id, TraceContext, <<"">>)},
        {<<"beamline.flow_id">>, maps:get(flow_id, TraceContext, <<"">>)},
        {<<"beamline.step_id">>, maps:get(step_id, TraceContext, <<"">>)},
        {<<"beamline.idempotency_key">>, maps:get(idempotency_key, TraceContext, <<"">>)}
    ],
    
    %% Start span with enhanced context
    opentelemetry:start_span(SpanName, #{attributes => Attributes}).
```

#### Phase 1 Deliverables
- ✅ Router extracts optional Orchestrator fields from incoming messages
- ✅ Enhanced idempotency logic using `idempotency_key` when available
- ✅ Trace spans include Orchestrator field attributes
- ✅ Backward compatibility maintained (fallbacks to existing behavior)
- ✅ All existing tests pass

### Phase 2: Gateway Enhancement (CP2) - External Field Passing

**Goal**: Gateway starts passing `run_id`, `flow_id`, `step_id`, `idempotency_key` from external world where possible.

**Timeline**: Week 2 of CP2
**Assigned to**: wrk-4 (Gateway/NestJS)

#### Required Gateway Module Modifications

**File**: `apps/gateway/src/dto/chat-completion.dto.ts`

**Changes**:
```typescript
// Add to ChatCompletionRequest class around line 20-25
export class ChatCompletionRequest {
  @ApiProperty({ description: 'The messages to generate completions for' })
  messages: ChatMessage[];

  @ApiPropertyOptional({ description: 'Model to use for completion' })
  model?: string;

  // NEW: Optional Orchestrator fields
  @ApiPropertyOptional({ description: 'Run identifier for tracing and orchestration' })
  run_id?: string;

  @ApiPropertyOptional({ description: 'Flow definition identifier' })
  flow_id?: string;

  @ApiPropertyOptional({ description: 'Step identifier within the flow' })
  step_id?: string;

  @ApiPropertyOptional({ description: 'Idempotency key for safe retries' })
  idempotency_key?: string;

  @ApiPropertyOptional({ description: 'Parent span ID for distributed tracing' })
  span_id?: string;

  // ... existing fields
}
```

**File**: `apps/gateway/src/services/chat-completion.service.ts`

**Changes**:
```typescript
// Modify createChatCompletion method around line 35-40
async createChatCompletion(
  request: ChatCompletionRequest,
  tenantId: string,
  traceId: string,
): Promise<Observable<ChatCompletionResponse>> {
  
  // NEW: Extract Orchestrator fields from request
  const orchestratorFields = {
    run_id: request.run_id || this.generateRunId(),
    flow_id: request.flow_id || 'chat-completion-flow',
    step_id: request.step_id || this.generateStepId(),
    idempotency_key: request.idempotency_key || this.generateIdempotencyKey(tenantId),
    span_id: request.span_id || this.generateSpanId(),
  };

  // Create message with enhanced context
  const message: Message = {
    message_id: this.generateMessageId(),
    tenant_id: tenantId,
    trace_id: traceId,
    message_type: 'chat.completion',
    payload: Buffer.from(JSON.stringify(request.messages)),
    metadata: {
      model: request.model || 'gpt-3.5-turbo',
      temperature: String(request.temperature || 0.7),
      max_tokens: String(request.max_tokens || 1000),
      // NEW: Add Orchestrator fields to metadata as well
      ...orchestratorFields,
    },
    timestamp_ms: Date.now(),
    // NEW: Populate optional Orchestrator fields
    run_id: orchestratorFields.run_id,
    flow_id: orchestratorFields.flow_id,
    step_id: orchestratorFields.step_id,
    idempotency_key: orchestratorFields.idempotency_key,
    span_id: orchestratorFields.span_id,
  };

  // Continue with existing routing logic
  return this.routeMessage(message, request.stream || false);
}

// Add helper methods
private generateRunId(): string {
  return `run_${uuidv4()}`;
}

private generateStepId(): string {
  return `step_${uuidv4()}`;
}

private generateIdempotencyKey(tenantId: string): string {
  return `${tenantId}:${uuidv4()}`;
}

private generateSpanId(): string {
  return `span_${uuidv4().substring(0, 16)}`;
}
```

**File**: `apps/gateway/src/services/routes.service.ts`

**Changes**:
```typescript
// Modify routeMessage method around line 50-55
private async routeMessage(
  message: Message,
  stream: boolean,
): Promise<Observable<ChatCompletionResponse>> {
  
  // NEW: Enhanced RouteRequest with Orchestrator fields
  const routeRequest: RouteRequest = {
    message: message,
    policy_id: this.configService.get<string>('DEFAULT_POLICY_ID'),
    context: {
      stream: String(stream),
      gateway_version: '2.0',
      // NEW: Pass Orchestrator fields in context for Router processing
      run_id: message.run_id || '',
      flow_id: message.flow_id || '',
      step_id: message.step_id || '',
      idempotency_key: message.idempotency_key || '',
    },
    // NEW: Request-level idempotency key
    idempotency_key: message.idempotency_key,
  };

  // Continue with existing NATS publishing logic
  const subject = this.buildNatsSubject(message.tenant_id, message.message_type);
  return this.natsService.request(subject, routeRequest, stream);
}
```

**File**: `apps/gateway/src/interceptors/tracing.interceptor.ts`

**Changes**:
```typescript
// Modify intercept method around line 25-30
async intercept(context: ExecutionContext, next: CallHandler): Promise<Observable<any>> {
  const request = context.switchToHttp().getRequest();
  const traceId = request.headers['x-trace-id'] || this.generateTraceId();
  
  // NEW: Extract Orchestrator fields from headers
  const orchestratorContext = {
    trace_id: traceId,
    span_id: request.headers['x-span-id'] || this.generateSpanId(),
    run_id: request.headers['x-run-id'] || this.generateRunId(),
    flow_id: request.headers['x-flow-id'] || 'default-flow',
    step_id: request.headers['x-step-id'] || this.generateStepId(),
    idempotency_key: request.headers['x-idempotency-key'] || this.generateIdempotencyKey(request.tenant?.id),
  };

  // Store in async local storage for downstream services
  this.tracingService.setContext(orchestratorContext);

  // Continue with existing tracing logic
  return next.handle().pipe(
    tap(() => {
      this.logger.log('Request completed', {
        trace_id: traceId,
        ...orchestratorContext,
      });
    }),
  );
}
```

#### Phase 2 Deliverables
- ✅ Gateway accepts optional Orchestrator fields in API requests
- ✅ Gateway generates Orchestrator fields when not provided by client
- ✅ Enhanced RouteRequest includes Orchestrator context
- ✅ Request-level idempotency key support
- ✅ Tracing interceptor propagates Orchestrator fields
- ✅ All existing API endpoints remain backward compatible

### Phase 3: Orchestrator/Worker Integration (CP3+) - Primary Source of Truth

**Goal**: Orchestrator/Worker services rely on these fields as primary source of truth.

**Timeline**: CP3+ (separate checkpoint)
**Assigned to**: wrk-1 (ABI/Contracts) + wrk-3 (Orchestrator/Erlang)

#### Phase 3 Implementation Plan

**Step 1**: Create new Orchestrator protobuf package
**File**: `proto/beamline/orchestrator/v1/orchestrator.proto`

**Structure**:
```protobuf
syntax = "proto3";

package beamline.orchestrator.v1;

// Orchestrator service for flow management
service OrchestratorService {
  rpc StartRun(StartRunRequest) returns (StartRunResponse);
  rpc QueryStatus(QueryStatusRequest) returns (QueryStatusResponse);
  rpc StreamEvents(StreamEventsRequest) returns (stream ExecutionEvent);
  rpc PauseRun(PauseRunRequest) returns (PauseRunResponse);
  rpc ResumeRun(ResumeRunRequest) returns (ResumeRunResponse);
}

// Worker service for step execution
service WorkerService {
  rpc ExecuteStep(ExecuteStepRequest) returns (ExecuteStepResponse);
  rpc ExecuteStepStream(ExecuteStepRequest) returns (stream StepChunk);
  rpc Heartbeat(HeartbeatRequest) returns (HeartbeatResponse);
  rpc AdvertiseCapabilities(AdvertiseCapabilitiesRequest) returns (AdvertiseCapabilitiesResponse);
}

// Base message with required Orchestrator fields
message OrchestratorMessage {
  string run_id = 1;              // Required: Unique run identifier
  string flow_id = 2;             // Required: Flow definition identifier
  string step_id = 3;             // Required: Step identifier
  string tenant_id = 4;             // Required: Tenant identifier
  string idempotency_key = 5;       // Required: Idempotency key
  string trace_id = 6;              // Optional: Trace identifier
  string span_id = 7;               // Optional: Span identifier
  map<string, string> metadata = 8; // Optional: Additional metadata
  int64 timestamp_ms = 9;           // Optional: Timestamp
}

message StartRunRequest {
  OrchestratorMessage base = 1;     // Required base fields
  string flow_definition = 2;       // JSON flow definition
  map<string, string> initial_context = 3;
}

message ExecuteStepRequest {
  OrchestratorMessage base = 1;     // Required base fields
  string step_type = 2;             // Type of step to execute
  bytes step_config = 3;              // Step configuration
  string blob_uri = 4;              // Optional: Large artifact reference
}

// ... additional message definitions
```

**Step 2**: Router bridge implementation
**File**: `apps/otp/router/src/router_orchestrator_bridge.erl`

**Implementation**:
```erlang
%% Bridge beamline.flow.v1 -> beamline.orchestrator.v1
bridge_flow_to_orchestrator(FlowMessage) ->
    %% Extract required fields from flow message
    BaseMessage = #{
        run_id => maps:get(run_id, FlowMessage, maps:get(message_id, FlowMessage)),
        flow_id => maps:get(flow_id, FlowMessage, "default-flow"),
        step_id => maps:get(step_id, FlowMessage, maps:get(message_id, FlowMessage)),
        tenant_id => maps:get(tenant_id, FlowMessage),
        idempotency_key => maps:get(idempotency_key, FlowMessage, maps:get(message_id, FlowMessage)),
        trace_id => maps:get(trace_id, FlowMessage, undefined),
        span_id => maps:get(span_id, FlowMessage, undefined),
        metadata => maps:get(metadata, FlowMessage, #{}),
        timestamp_ms => maps:get(timestamp_ms, FlowMessage, erlang:system_time(millisecond))
    },
    
    %% Create Orchestrator message
    #'OrchestratorMessage'{
        run_id = maps:get(run_id, BaseMessage),
        flow_id = maps:get(flow_id, BaseMessage),
        step_id = maps:get(step_id, BaseMessage),
        tenant_id = maps:get(tenant_id, BaseMessage),
        idempotency_key = maps:get(idempotency_key, BaseMessage),
        trace_id = maps:get(trace_id, BaseMessage),
        span_id = maps:get(span_id, BaseMessage),
        metadata = maps:get(metadata, BaseMessage),
        timestamp_ms = maps:get(timestamp_ms, BaseMessage)
    }.
```

#### Phase 3 Deliverables
- ✅ New `beamline.orchestrator.v1` package with required fields
- ✅ Orchestrator and Worker service definitions
- ✅ Router bridge for protocol conversion
- ✅ All fields become required in Orchestrator context
- ✅ Backward compatibility maintained through Router bridging
- ✅ Migration guide for CP3+ services

## Migration Strategy Summary

### Field Evolution Path

| Phase | Router (beamline.flow.v1) | Gateway (API) | Orchestrator (beamline.orchestrator.v1) |
|-------|---------------------------|---------------|----------------------------------------|
| CP1 | Basic fields only | Basic request/response | Not present |
| CP2 | ✅ Optional fields added | ✅ Optional fields supported | Not present |
| CP3 | ✅ Bridge to Orchestrator | ✅ Bridge to Orchestrator | ✅ Required fields enforced |

### Backward Compatibility Matrix

| Client Version | Router CP2 | Gateway CP2 | Router CP3 | Gateway CP3 |
|----------------|------------|-------------|------------|-------------|
| CP1 Client | ✅ Works | ✅ Works | ✅ Works (bridge) | ✅ Works (bridge) |
| CP2 Client | ✅ Works | ✅ Works | ✅ Works (bridge) | ✅ Works (bridge) |
| CP3 Client | ❌ Not supported | ❌ Not supported | ✅ Works native | ✅ Works native |

## Implementation Assignments

### wrk-1 (ABI/Contracts) - Phase 3
- Create `proto/beamline/orchestrator/v1/orchestrator.proto`
- Define all service contracts and message types
- Ensure protobuf backward compatibility
- Update `buf` configuration and validation rules

### wrk-3 (Router/Erlang) - Phase 1 + 3
- Modify `router_nats_subscriber.erl` for optional field extraction
- Update `router_idempotency.erl` for enhanced idempotency
- Enhance `router_trace.erl` for Orchestrator field tracing
- Implement `router_orchestrator_bridge.erl` for CP3 bridging

### wrk-4 (Gateway/NestJS) - Phase 2
- Extend `ChatCompletionRequest` DTO with Orchestrator fields
- Update `ChatCompletionService` for field generation and propagation
- Enhance `RoutesService` for enhanced RouteRequest creation
- Modify `TracingInterceptor` for Orchestrator context propagation

## Risk Mitigation

### Risk 1: Performance Impact
**Mitigation**: Optional fields are only processed when present, no performance degradation for existing traffic.

### Risk 2: Memory Usage
**Mitigation**: Enhanced trace context is garbage collected after message processing completes.

### Risk 3: Migration Complexity
**Mitigation**: Gradual 3-phase approach allows rollback at any phase without breaking existing functionality.

## Success Metrics

- ✅ Zero breaking changes across all phases
- ✅ All existing tests pass without modification
- ✅ New optional fields work end-to-end in CP2
- ✅ Bridge functionality works in CP3
- ✅ Performance benchmarks maintain <5% overhead
- ✅ Memory usage increase <10% for enhanced tracing

## Next Steps

1. **wrk-1**: Review and approve protobuf schema changes
2. **wrk-3**: Implement Phase 1 Router modifications
3. **wrk-4**: Implement Phase 2 Gateway enhancements  
4. **All teams**: Coordinate CP3+ Orchestrator package creation
5. **QA**: Validate backward compatibility across all phases

This implementation plan provides concrete, executable steps for each worker team to implement the ABI field migration while maintaining full backward compatibility and enabling future Orchestrator integration.
# Protocol Buffers Definitions

This directory contains Protocol Buffers (protobuf) definitions for BeamLine Constructor services.

## Structure

```
proto/
├── buf.yaml              # Buf configuration for linting and breaking changes
├── buf.gen.yaml          # Code generation configuration
├── beamline/
│   ├── flow/v1/
│   │   └── flow.proto    # Router.Decide service definitions
│   └── provider/v1/
│       └── provider.proto # Provider service definitions
└── README.md             # This file
```

## Proto Source Files Status

**Current Status**: Proto source files (`proto/beamline/flow/v1/flow.proto`, `proto/beamline/provider/v1/provider.proto`) are currently **missing** (directories exist but are empty).

**Source of Truth**: Router uses generated code from `apps/otp/router/src/flow_pb.erl` and `apps/otp/router/include/flow_pb.hrl` as the authoritative source for message definitions.

**To Restore Proto Files**:
1. Extract Proto definitions from generated code (`flow_pb.erl`)
2. Recreate `.proto` files with proper protobuf syntax
3. Validate with `buf lint` and `buf build`
4. Regenerate code to verify: `rebar3 gpb compile`

**Note**: This restoration is deferred to CP2-LC. For CP1, generated code is the source of truth.

## CAF Worker Contracts (ExecAssignment / ExecResult)

**Current Status**: Proto definitions for `ExecAssignment` and `ExecResult` are currently **missing** (not yet defined in proto files).

**Source of Truth**: JSON contract definitions in `docs/API_CONTRACTS.md`:
- `ExecAssignment`: Router → Worker assignment (JSON format)
- `ExecAssignmentAck`: Worker → Router acknowledgment (JSON format)
- `ExecResult`: Worker → Router execution result (JSON format)

**CP1 StepResult Contract**: CAF Worker internally uses a unified `StepResult` type (C++ struct) for all block executions. This type is converted to `ExecResult` JSON format via `ResultConverter::to_exec_result_json()` before publishing to NATS.

**StepResult → ExecResult Mapping**:
- `StepStatus::ok` → `ExecResult.status = "success"`
- `StepStatus::error` → `ExecResult.status = "error"`
- `StepStatus::timeout` → `ExecResult.status = "timeout"`
- `StepStatus::cancelled` → `ExecResult.status = "cancelled"`
- `ErrorCode` (1xxx-5xxx) → `ExecResult.error_code` (string format)
- `ResultMetadata` (trace_id, flow_id, step_id, tenant_id) → `ExecResult` correlation fields

**StepResult Contract Definition**: See `apps/caf/processor/docs/ARCHITECTURE_ROLE.md#43-stepresult-contract-cp1-invariant` for complete contract specification.

**Implementation**:
- StepResult type: `apps/caf/processor/include/beamline/worker/core.hpp`
- ResultConverter: `apps/caf/processor/include/beamline/worker/result_converter.hpp`

**Future Proto Definitions** (CP2+):
- `ExecAssignment` proto message (to be defined)
- `ExecAssignmentAck` proto message (to be defined)
- `ExecResult` proto message (to be defined)
- Proto definitions will align with current JSON contracts and StepResult mapping

## Services

### Router Service (`beamline.flow.v1`)

**Service**: `Router.Decide`

**Messages**:
- `Message`: Incoming message for routing
  - **CP2+ Optional Fields**: `run_id`, `flow_id`, `step_id`, `idempotency_key`, `span_id` (fields 8-12)
- `RouteRequest`: Routing request with message and context
  - **CP2+ Optional Field**: `idempotency_key` (field 4)
- `RouteDecision`: Routing decision with selected provider

**Usage**:
- **gRPC**: Direct service call from Gateway to Router
- **NATS**: Publish to `beamline.router.v1.decide`, receive reply

**NATS JSON Payload Format**:

Request (matches `RouteRequest`):
```json
{
  "message": {
    "message_id": "msg_1234567890",
    "tenant_id": "tenant_abc123",
    "trace_id": "trace_abc123",
    "message_type": "chat",
    "payload": "{\"text\": \"Hello, world!\"}",
    "metadata": {"source": "gateway"},
    "timestamp_ms": 1704067200000,
    "run_id": "run_abc123",
    "flow_id": "flow_xyz789",
    "step_id": "step_012345",
    "idempotency_key": "idem_key_345",
    "span_id": "span_678901"
  },
  "policy_id": "default",
  "context": {"user_id": "user_123"},
  "idempotency_key": "request_idem_key_456"
}
```

**Note**: CP2+ optional fields (`run_id`, `flow_id`, `step_id`, `idempotency_key`, `span_id` in `Message`, and `idempotency_key` in `RouteRequest`) are backward compatible. Existing CP1 clients can omit these fields.

Response (matches `RouteDecision`):
```json
{
  "provider_id": "openai",
  "reason": "weighted",
  "priority": 50,
  "expected_latency_ms": 250,
  "expected_cost": 0.001,
  "metadata": {"algorithm": "weighted_round_robin"}
}
```

## Code Generation

### Prerequisites

Install `buf`:
```bash
# macOS
brew install bufbuild/buf/buf

# Linux
curl -sSL "https://github.com/bufbuild/buf/releases/latest/download/buf-$(uname -s)-$(uname -m)" -o "/usr/local/bin/buf"
chmod +x /usr/local/bin/buf
```

### Generate Code

**Erlang/OTP**:
```bash
cd proto
buf generate --template buf.gen.yaml
```

**TypeScript**:
```bash
cd proto
buf generate --template buf.gen.yaml
```

### Validation

**Lint**:
```bash
cd proto
buf lint
```

**Breaking Changes Check**:
```bash
cd proto
buf breaking --against '.git#branch=main'
```

**Build**:
```bash
cd proto
buf build
```

## Compatibility

### NATS JSON Payload

NATS messages use JSON format that must match protobuf structures:

1. **Field Names**: Use snake_case (matches protobuf field names)
2. **Types**:
   - `string` → JSON string
   - `int32`, `int64` → JSON number
   - `double` → JSON number
   - `bytes` → JSON string (base64 or UTF-8)
   - `map<string, string>` → JSON object
   - `Message` → JSON object

3. **Payload Field**: 
   - In protobuf: `bytes payload`
   - In NATS JSON: `"payload": "{\"text\": \"...\"}"` (string)
   - Conversion: JSON string → bytes (UTF-8 encoding)

### TypeScript DTOs

Gateway TypeScript DTOs should match protobuf structures:

- `MessageDto` ↔ `Message`
- `RouteRequestDto` ↔ `RouteRequest`
- `RouteDecisionDto` ↔ `RouteDecision`

**Note**: `payload` field:
- Protobuf: `bytes` (binary)
- TypeScript DTO: `string` (JSON string)
- Conversion handled in Gateway service layer

## Versioning

- **Package**: `beamline.flow.v1` (version suffix in package name)
- **Breaking Changes**: Use new package version (e.g., `v2`)
- **Non-Breaking**: Update within same version

## References

- [Buf Documentation](https://docs.buf.build)
- [Protocol Buffers Guide](https://developers.google.com/protocol-buffers)
- [NATS Subjects](docs/NATS_SUBJECTS.md)
- [API Registry](docs/ARCHITECTURE/api-registry.md)
- [Orchestrator ABI Bridge](docs/archive/dev/ORCHESTRATOR_ROUTER_ABI_BRIDGE.md) - CP2+ Orchestrator fields migration guide


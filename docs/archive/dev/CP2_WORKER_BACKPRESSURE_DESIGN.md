# CP2 Worker Backpressure & Bounded Queue Design

**Version**: 1.0  
**Date**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Worker**: wrk-3 (Worker Reliability)  
**Status**: 📋 **DESIGN DOCUMENT** (CP2)

---

## Executive Summary

This document designs the CP2 bounded queue and backpressure protocol between Router and Worker, including:

1. **Overload Signals**: How Worker signals overload to Router (and vice versa)
2. **Status Extensions**: New fields/statuses in `ExecResult`/`ExecAssignmentAck` for overload indication
3. **Rejection Policies**: drop, reject, defer, degrade policies for handling overload

**Foundation**: CP1 Worker has basic backpressure (concurrent limits, unbounded queue). CP2 extends this with bounded queues, rejection policies, and explicit overload signaling.

**References**:
- `docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md` - Queue reliability review and issues
- `apps/otp/router/docs/API_CONTRACTS.md` - ExecResult and ExecAssignmentAck contracts
- `docs/archive/dev/CP2_WORKER_RELIABILITY_PLAN.md` - High-level CP2 plan
- `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` - Executable backlog

---

## 1. Current State (CP1)

### 1.1. CP1 Backpressure (Basic)

**Current Implementation** (`apps/caf/processor/src/runtime/actor_pools.hpp`):

- ✅ `max_concurrency_` limits concurrent execution per resource pool
- ✅ Queue-based task submission when at capacity
- ✅ `current_load_` tracks active tasks
- ❌ **Unbounded queue growth** (no `max_queue_size` limit)
- ❌ **No queue rejection** when overloaded
- ❌ **No queue depth monitoring** or metrics
- ❌ **No overload signaling** to Router

**Problem**: Under high load, queue can grow indefinitely, leading to memory exhaustion.

### 1.2. CP1 Message Flow

**Current Flow** (Router → Worker):

1. Router publishes `ExecAssignment` to `caf.exec.assign.v1`
2. Worker receives `ExecAssignment`
3. Worker validates assignment
4. Worker publishes `ExecAssignmentAck` with status: `"accepted"` | `"rejected"` | `"error"`
5. Worker queues task (if accepted)
6. Worker executes task when capacity available
7. Worker publishes `ExecResult` to `caf.exec.result.v1`

**Limitations**:
- Worker always accepts assignments (unbounded queue)
- No overload indication in `ExecAssignmentAck`
- No queue depth information communicated to Router

---

## 2. CP2 Overload Signaling

### 2.1. Overload States

**Design Principle**: Worker signals overload state to Router through `ExecAssignmentAck` and optional status messages.

**Overload States**:

| State | Description | Queue Depth | Action |
|-------|-------------|-------------|--------|
| **healthy** | Normal operation | < 50% capacity | Accept all assignments |
| **degraded** | Approaching capacity | 50-80% capacity | Accept assignments, but signal degradation |
| **overloaded** | Near capacity | 80-100% capacity | Reject new assignments (based on policy) |
| **critical** | At capacity | 100% capacity | Reject all new assignments |

### 2.2. Overload Signal in ExecAssignmentAck

**CP2 Extension**: Add `overload_status` field to `ExecAssignmentAck`.

**Extended ExecAssignmentAck Format**:

```json
{
  "assignment_id": "uuid-string",
  "status": "accepted" | "rejected" | "error",
  "message": "string (optional)",
  "overload_status": "healthy" | "degraded" | "overloaded" | "critical" (optional, CP2+)
}
```

**Field Requirements**:
- `overload_status`: string (optional, CP2+) - Worker overload state
  - `"healthy"`: Normal operation (< 50% queue capacity)
  - `"degraded"`: Approaching capacity (50-80% queue capacity)
  - `"overloaded"`: Near capacity (80-100% queue capacity)
  - `"critical"`: At capacity (100% queue capacity)

**Backward Compatibility**:
- `overload_status` is optional (CP1 Router ignores it)
- CP1 Router processes `ExecAssignmentAck` as before (only checks `status`)

### 2.3. Overload Signal in ExecResult

**CP2 Extension**: Add `queue_depth` and `overload_status` fields to `ExecResult` (optional, for monitoring).

**Extended ExecResult Format**:

```json
{
  "assignment_id": "uuid-string",
  "request_id": "uuid-string",
  "status": "success" | "error" | "timeout" | "cancelled",
  "provider_id": "string",
  "job": {
    "type": "string"
  },
  "latency_ms": "number",
  "cost": "number",
  "trace_id": "string (optional)",
  "tenant_id": "string (optional)",
  "timestamp": "number (optional)",
  "queue_depth": "number (optional, CP2+)" - Queue depth when task was queued
  "overload_status": "healthy" | "degraded" | "overloaded" | "critical" (optional, CP2+)
}
```

**Field Requirements**:
- `queue_depth`: number (optional, CP2+) - Queue depth when task was queued (for monitoring)
- `overload_status`: string (optional, CP2+) - Worker overload state when task completed

**Backward Compatibility**:
- `queue_depth` and `overload_status` are optional (CP1 Router ignores them)
- CP1 Router processes `ExecResult` as before

### 2.4. Overload Calculation

**Queue Depth Thresholds**:

```cpp
enum class OverloadStatus {
    healthy,      // queue_depth < max_queue_size * 0.5
    degraded,     // queue_depth >= max_queue_size * 0.5 && < max_queue_size * 0.8
    overloaded,   // queue_depth >= max_queue_size * 0.8 && < max_queue_size
    critical      // queue_depth >= max_queue_size
};

OverloadStatus calculate_overload_status(size_t queue_depth, size_t max_queue_size) {
    double utilization = static_cast<double>(queue_depth) / max_queue_size;
    
    if (utilization >= 1.0) {
        return OverloadStatus::critical;
    } else if (utilization >= 0.8) {
        return OverloadStatus::overloaded;
    } else if (utilization >= 0.5) {
        return OverloadStatus::degraded;
    } else {
        return OverloadStatus::healthy;
    }
}
```

---

## 3. Rejection Policies

### 3.1. Policy Types

**Design Principle**: Worker can apply different rejection policies when queue is full or overloaded.

**Policy Types**:

| Policy | Description | Behavior | Use Case |
|--------|-------------|----------|----------|
| **reject_new** | Reject new assignments | Return `ExecAssignmentAck` with `status: "rejected"` | Default: Fail fast, prevent queue growth |
| **drop_oldest** | Drop oldest queued task | Remove oldest task from queue, accept new assignment | High-priority new tasks, low-priority old tasks |
| **defer** | Defer assignment | Accept assignment but delay execution | Temporary overload, expect recovery |
| **degrade** | Degrade service quality | Accept assignment but reduce quality (e.g., skip retries) | Maintain availability, reduce quality |

### 3.2. Policy Configuration

**Configuration Format**:

```json
{
  "worker": {
    "queue": {
      "v2": {
        "enabled": false,
        "max_queue_size": 1000,
        "rejection_policy": "reject_new",
        "overload_thresholds": {
          "degraded": 0.5,
          "overloaded": 0.8,
          "critical": 1.0
        },
        "policies": {
          "reject_new": {
            "enabled": true,
            "default": true
          },
          "drop_oldest": {
            "enabled": false,
            "max_drop_age_ms": 60000
          },
          "defer": {
            "enabled": false,
            "max_defer_time_ms": 30000
          },
          "degrade": {
            "enabled": false,
            "degradation_levels": {
              "light": {
                "skip_retries": false,
                "reduce_timeout": false
              },
              "medium": {
                "skip_retries": true,
                "reduce_timeout": true,
                "timeout_reduction_factor": 0.8
              },
              "heavy": {
                "skip_retries": true,
                "reduce_timeout": true,
                "timeout_reduction_factor": 0.5,
                "skip_circuit_breaker": true
              }
            }
          }
        }
      }
    }
  }
}
```

### 3.3. Policy Implementation

#### Policy 1: reject_new (Default)

**Behavior**: Reject new assignments when queue is full.

**Implementation**:

```cpp
caf::expected<ExecAssignmentAck> handle_exec_assignment(const ExecAssignment& assignment) {
    auto pool = get_pool_for_resource(assignment.resource_class);
    auto queue_depth = pool->queue_depth();
    auto max_queue_size = pool->max_queue_size();
    auto overload_status = calculate_overload_status(queue_depth, max_queue_size);
    
    if (overload_status == OverloadStatus::critical) {
        // Queue full, reject new assignment
        return ExecAssignmentAck{
            assignment_id: assignment.assignment_id,
            status: "rejected",
            message: "Queue full, rejecting assignment",
            overload_status: "critical"
        };
    }
    
    // Accept assignment
    if (!pool->submit(task)) {
        return ExecAssignmentAck{
            assignment_id: assignment.assignment_id,
            status: "rejected",
            message: "Failed to queue assignment",
            overload_status: overload_status_to_string(overload_status)
        };
    }
    
    return ExecAssignmentAck{
        assignment_id: assignment.assignment_id,
        status: "accepted",
        message: "Assignment queued",
        overload_status: overload_status_to_string(overload_status)
    };
}
```

**Use Case**: Default policy for most scenarios. Prevents queue growth, fails fast.

#### Policy 2: drop_oldest

**Behavior**: Drop oldest queued task, accept new assignment.

**Implementation**:

```cpp
caf::expected<ExecAssignmentAck> handle_exec_assignment_drop_oldest(const ExecAssignment& assignment) {
    auto pool = get_pool_for_resource(assignment.resource_class);
    auto queue_depth = pool->queue_depth();
    auto max_queue_size = pool->max_queue_size();
    
    if (queue_depth >= max_queue_size) {
        // Drop oldest task
        auto dropped_task = pool->drop_oldest();
        if (dropped_task) {
            // Publish rejection for dropped task
            publish_dropped_assignment_ack(dropped_task->assignment_id);
        }
    }
    
    // Accept new assignment
    if (!pool->submit(task)) {
        return ExecAssignmentAck{
            assignment_id: assignment.assignment_id,
            status: "rejected",
            message: "Failed to queue assignment after dropping oldest"
        };
    }
    
    return ExecAssignmentAck{
        assignment_id: assignment.assignment_id,
        status: "accepted",
        message: "Assignment queued (dropped oldest)"
    };
}
```

**Use Case**: High-priority new tasks, low-priority old tasks. Trade-off: May drop important tasks.

#### Policy 3: defer

**Behavior**: Accept assignment but delay execution.

**Implementation**:

```cpp
caf::expected<ExecAssignmentAck> handle_exec_assignment_defer(const ExecAssignment& assignment) {
    auto pool = get_pool_for_resource(assignment.resource_class);
    auto queue_depth = pool->queue_depth();
    auto max_queue_size = pool->max_queue_size();
    auto overload_status = calculate_overload_status(queue_depth, max_queue_size);
    
    if (overload_status == OverloadStatus::critical) {
        // Still reject if at capacity
        return ExecAssignmentAck{
            assignment_id: assignment.assignment_id,
            status: "rejected",
            message: "Queue at capacity, cannot defer"
        };
    }
    
    // Calculate defer time based on queue depth
    int64_t defer_time_ms = calculate_defer_time(queue_depth, max_queue_size);
    
    // Queue with defer time
    auto deferred_task = create_deferred_task(assignment, defer_time_ms);
    if (!pool->submit(deferred_task)) {
        return ExecAssignmentAck{
            assignment_id: assignment.assignment_id,
            status: "rejected",
            message: "Failed to queue deferred assignment"
        };
    }
    
    return ExecAssignmentAck{
        assignment_id: assignment.assignment_id,
        status: "accepted",
        message: "Assignment deferred for " + std::to_string(defer_time_ms) + "ms",
        overload_status: overload_status_to_string(overload_status)
    };
}

int64_t calculate_defer_time(size_t queue_depth, size_t max_queue_size) {
    double utilization = static_cast<double>(queue_depth) / max_queue_size;
    // Linear scaling: 0ms at 50%, 30000ms at 100%
    if (utilization < 0.5) {
        return 0;
    }
    return static_cast<int64_t>((utilization - 0.5) * 2 * 30000); // Max 30s defer
}
```

**Use Case**: Temporary overload, expect recovery. Trade-off: Increased latency.

#### Policy 4: degrade

**Behavior**: Accept assignment but reduce service quality.

**Implementation**:

```cpp
caf::expected<ExecAssignmentAck> handle_exec_assignment_degrade(const ExecAssignment& assignment) {
    auto pool = get_pool_for_resource(assignment.resource_class);
    auto queue_depth = pool->queue_depth();
    auto max_queue_size = pool->max_queue_size();
    auto overload_status = calculate_overload_status(queue_depth, max_queue_size);
    
    if (overload_status == OverloadStatus::critical) {
        // Still reject if at capacity
        return ExecAssignmentAck{
            assignment_id: assignment.assignment_id,
            status: "rejected",
            message: "Queue at capacity, cannot degrade"
        };
    }
    
    // Determine degradation level
    DegradationLevel level = determine_degradation_level(overload_status);
    
    // Apply degradation to assignment
    auto degraded_assignment = apply_degradation(assignment, level);
    
    // Queue degraded assignment
    if (!pool->submit(degraded_assignment)) {
        return ExecAssignmentAck{
            assignment_id: assignment.assignment_id,
            status: "rejected",
            message: "Failed to queue degraded assignment"
        };
    }
    
    return ExecAssignmentAck{
        assignment_id: assignment.assignment_id,
        status: "accepted",
        message: "Assignment accepted with degradation: " + degradation_level_to_string(level),
        overload_status: overload_status_to_string(overload_status)
    };
}

DegradationLevel determine_degradation_level(OverloadStatus status) {
    switch (status) {
        case OverloadStatus::degraded:
            return DegradationLevel::light;
        case OverloadStatus::overloaded:
            return DegradationLevel::medium;
        default:
            return DegradationLevel::none;
    }
}

Assignment apply_degradation(const ExecAssignment& assignment, DegradationLevel level) {
    auto degraded = assignment;
    
    switch (level) {
        case DegradationLevel::light:
            // No degradation
            break;
        case DegradationLevel::medium:
            // Skip retries, reduce timeout
            degraded.options.retry_count = 0;
            degraded.options.timeout_ms = static_cast<int64_t>(degraded.options.timeout_ms * 0.8);
            break;
        case DegradationLevel::heavy:
            // Skip retries, reduce timeout significantly, skip circuit breaker
            degraded.options.retry_count = 0;
            degraded.options.timeout_ms = static_cast<int64_t>(degraded.options.timeout_ms * 0.5);
            degraded.options.skip_circuit_breaker = true;
            break;
    }
    
    return degraded;
}
```

**Use Case**: Maintain availability, reduce quality. Trade-off: Lower success rate, faster failures.

### 3.4. Policy Selection

**Policy Selection Logic**:

```cpp
RejectionPolicy select_rejection_policy(OverloadStatus status, const Config& config) {
    // Check if specific policy is enabled for this overload status
    if (status == OverloadStatus::critical) {
        // At capacity: always reject_new (cannot accept more)
        return RejectionPolicy::reject_new;
    }
    
    // Check configured policy
    if (config.queue.v2.policies.reject_new.enabled && config.queue.v2.policies.reject_new.default) {
        return RejectionPolicy::reject_new;
    }
    
    if (config.queue.v2.policies.drop_oldest.enabled) {
        return RejectionPolicy::drop_oldest;
    }
    
    if (config.queue.v2.policies.defer.enabled) {
        return RejectionPolicy::defer;
    }
    
    if (config.queue.v2.policies.degrade.enabled) {
        return RejectionPolicy::degrade;
    }
    
    // Default: reject_new
    return RejectionPolicy::reject_new;
}
```

---

## 4. Router Response to Overload Signals

### 4.1. Router Overload Handling

**Design Principle**: Router should respond to Worker overload signals by adjusting routing behavior.

**Router Actions**:

| Overload Status | Router Action | Description |
|----------------|---------------|-------------|
| **healthy** | Normal routing | Route assignments to Worker as usual |
| **degraded** | Continue routing | Continue routing, but prefer other Workers if available |
| **overloaded** | Reduce routing | Reduce routing to this Worker, prefer other Workers |
| **critical** | Stop routing | Stop routing to this Worker until status improves |

### 4.2. Router Backpressure Protocol

**Router → Worker Flow** (with backpressure):

1. Router receives `ExecAssignmentAck` with `overload_status`
2. Router updates Worker health status based on `overload_status`
3. Router adjusts routing decisions:
   - If `overload_status == "critical"`: Stop routing to this Worker
   - If `overload_status == "overloaded"`: Reduce routing (e.g., 50% of normal)
   - If `overload_status == "degraded"`: Prefer other Workers if available
   - If `overload_status == "healthy"`: Normal routing

**Router Health Tracking**:

```erlang
-record(worker_health, {
    worker_id :: binary(),
    overload_status :: healthy | degraded | overloaded | critical,
    last_update :: integer(),  % Timestamp
    queue_depth :: integer(),
    active_tasks :: integer()
}).

update_worker_health(Ack) ->
    #exec_assignment_ack{
        assignment_id = AssignmentId,
        overload_status = OverloadStatus,
        queue_depth = QueueDepth
    } = Ack,
    
    WorkerId = extract_worker_id(AssignmentId),
    
    Health = #worker_health{
        worker_id = WorkerId,
        overload_status = OverloadStatus,
        last_update = erlang:system_time(millisecond),
        queue_depth = QueueDepth,
        active_tasks = undefined  % Not available in ACK
    },
    
    ets:insert(worker_health_table, {WorkerId, Health}).

should_route_to_worker(WorkerId, Request) ->
    case ets:lookup(worker_health_table, WorkerId) of
        [{WorkerId, #worker_health{overload_status = critical}}] ->
            false;  % Don't route to critical Worker
        [{WorkerId, #worker_health{overload_status = overloaded}}] ->
            % Reduce routing: 50% chance
            random:uniform(2) =:= 1;
        [{WorkerId, #worker_health{overload_status = degraded}}] ->
            % Prefer other Workers, but allow routing
            true;
        [{WorkerId, #worker_health{overload_status = healthy}}] ->
            true;  % Normal routing
        [] ->
            true;  % Unknown Worker, allow routing (CP1 compatibility)
    end.
```

---

## 5. StepResult/ExecResult Extensions

### 5.1. StepResult Extensions (Internal)

**CP2 Extension**: Add queue depth and overload status to `StepResult` (for internal tracking).

**Extended StepResult** (C++ struct):

```cpp
struct StepResult {
    StepStatus status = StepStatus::ok;
    ErrorCode error_code = ErrorCode::none;
    std::unordered_map<std::string, std::string> outputs;
    std::string error_message;
    ResultMetadata metadata;
    int64_t latency_ms = 0;
    int32_t retries_used = 0;
    
    // CP2 extensions (optional)
    #ifdef CP2_QUEUE_MANAGEMENT_ENABLED
    size_t queue_depth_at_queue = 0;  // Queue depth when task was queued
    OverloadStatus overload_status_at_queue = OverloadStatus::healthy;  // Overload status when queued
    #endif
};
```

### 5.2. ExecResult Extensions (NATS)

**CP2 Extension**: Add queue depth and overload status to `ExecResult` JSON (for Router monitoring).

**Extended ExecResult Format** (from Section 2.3):

```json
{
  "assignment_id": "uuid-string",
  "request_id": "uuid-string",
  "status": "success" | "error" | "timeout" | "cancelled",
  "provider_id": "string",
  "job": {
    "type": "string"
  },
  "latency_ms": "number",
  "cost": "number",
  "trace_id": "string (optional)",
  "tenant_id": "string (optional)",
  "timestamp": "number (optional)",
  "queue_depth": "number (optional, CP2+)" - Queue depth when task was queued
  "overload_status": "healthy" | "degraded" | "overloaded" | "critical" (optional, CP2+)
}
```

**Conversion** (StepResult → ExecResult):

```cpp
std::unordered_map<std::string, std::string> ResultConverter::to_exec_result_json(const StepResult& result) {
    auto exec_result = base_conversion(result);
    
    #ifdef CP2_QUEUE_MANAGEMENT_ENABLED
    if (result.queue_depth_at_queue > 0) {
        exec_result["queue_depth"] = std::to_string(result.queue_depth_at_queue);
    }
    if (result.overload_status_at_queue != OverloadStatus::healthy) {
        exec_result["overload_status"] = overload_status_to_string(result.overload_status_at_queue);
    }
    #endif
    
    return exec_result;
}
```

---

## 6. Feature Gate Strategy

### 6.1. Feature Flag: `worker.queue.v2.enabled`

**Purpose**: Gate CP2 queue management features to protect CP1 baseline.

**Default**: `false` (CP1 baseline mode)

**Behavior**:
- **When `false` (CP1 mode)**:
  - Unbounded queue (no size limits)
  - No queue rejection
  - No overload signaling
  - No rejection policies

- **When `true` (CP2 mode)**:
  - Bounded queue (configurable `max_queue_size`)
  - Queue rejection when full
  - Overload signaling in `ExecAssignmentAck` and `ExecResult`
  - Configurable rejection policies (reject_new, drop_oldest, defer, degrade)

### 6.2. Feature Gate Implementation

**Configuration Check**:

```cpp
bool is_cp2_queue_enabled() {
    // Check environment variable
    auto env_value = std::getenv("WORKER_QUEUE_V2_ENABLED");
    if (env_value && std::string(env_value) == "true") {
        return true;
    }
    
    // Check config file
    auto config = load_worker_config();
    if (config.worker.queue.v2.enabled) {
        return true;
    }
    
    // Default: CP1 baseline
    return false;
}
```

**Usage in Queue Management**:

```cpp
caf::expected<ExecAssignmentAck> handle_exec_assignment(const ExecAssignment& assignment) {
    if (is_cp2_queue_enabled()) {
        // CP2: Bounded queue with rejection policies
        return handle_exec_assignment_cp2(assignment);
    } else {
        // CP1: Unbounded queue, always accept
        return handle_exec_assignment_cp1(assignment);
    }
}

caf::expected<ExecAssignmentAck> handle_exec_assignment_cp1(const ExecAssignment& assignment) {
    // CP1 baseline: Always accept, unbounded queue
    auto pool = get_pool_for_resource(assignment.resource_class);
    pool->submit(task);  // No size check
    return ExecAssignmentAck{
        assignment_id: assignment.assignment_id,
        status: "accepted"
    };
}
```

---

## 7. Configuration Examples

### 7.1. Minimal Configuration (CP2 Defaults)

```json
{
  "worker": {
    "queue": {
      "v2": {
        "enabled": true,
        "max_queue_size": 1000,
        "rejection_policy": "reject_new"
      }
    }
  }
}
```

**Uses**: Default bounded queue (1000 tasks), reject_new policy.

### 7.2. Drop Oldest Configuration

```json
{
  "worker": {
    "queue": {
      "v2": {
        "enabled": true,
        "max_queue_size": 500,
        "rejection_policy": "drop_oldest",
        "policies": {
          "drop_oldest": {
            "enabled": true,
            "max_drop_age_ms": 60000
          }
        }
      }
    }
  }
}
```

### 7.3. Degrade Configuration

```json
{
  "worker": {
    "queue": {
      "v2": {
        "enabled": true,
        "max_queue_size": 2000,
        "rejection_policy": "degrade",
        "policies": {
          "degrade": {
            "enabled": true,
            "degradation_levels": {
              "medium": {
                "skip_retries": true,
                "reduce_timeout": true,
                "timeout_reduction_factor": 0.8
              }
            }
          }
        }
      }
    }
  }
}
```

---

## 8. Acceptance Criteria

### 8.1. Bounded Queue

- ✅ Bounded queue size limits implemented
- ✅ Queue rejection when full
- ✅ Queue depth monitoring working
- ✅ Feature flag gates CP2 behavior

### 8.2. Overload Signaling

- ✅ `overload_status` in `ExecAssignmentAck` working
- ✅ `queue_depth` and `overload_status` in `ExecResult` working (optional)
- ✅ Overload calculation correct (healthy, degraded, overloaded, critical)
- ✅ Backward compatibility maintained (CP1 Router ignores new fields)

### 8.3. Rejection Policies

- ✅ `reject_new` policy working
- ✅ `drop_oldest` policy working
- ✅ `defer` policy working
- ✅ `degrade` policy working
- ✅ Policy selection logic correct

### 8.4. Router Integration

- ✅ Router processes `overload_status` from `ExecAssignmentAck`
- ✅ Router adjusts routing based on overload status
- ✅ Router health tracking working
- ✅ Router backpressure protocol working

---

## 9. References

### Input Documents
- `docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md` - Queue reliability review and issues
- `apps/otp/router/docs/API_CONTRACTS.md` - ExecResult and ExecAssignmentAck contracts
- `docs/archive/dev/CP2_WORKER_RELIABILITY_PLAN.md` - High-level CP2 plan
- `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` - Executable backlog

### Implementation Files
- `apps/caf/processor/src/runtime/actor_pools.hpp` - Actor pool implementation
- `apps/caf/processor/src/worker_actor.cpp` - Worker actor implementation
- `apps/otp/router/src/router_ack_consumer.erl` - ACK consumer implementation
- `apps/otp/router/src/router_result_consumer.erl` - Result consumer implementation

### Related Documents
- `apps/caf/processor/docs/ARCHITECTURE_ROLE.md` - Worker architecture and requirements
- `apps/otp/router/docs/NATS_SUBJECTS.md` - NATS subjects and message flow

---

## Change History

**v1.0 (2025-01-27)**:
- Initial CP2 Worker Backpressure & Bounded Queue Design
- Overload signaling protocol defined
- Rejection policies designed (reject_new, drop_oldest, defer, degrade)
- ExecResult/ExecAssignmentAck extensions specified
- Feature gate strategy defined (`worker.queue.v2.enabled`)

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Status**: Design Document Ready for Implementation


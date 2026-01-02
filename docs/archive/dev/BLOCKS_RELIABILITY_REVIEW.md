# Blocks Reliability Review

**Date**: 2025-01-27  
**Reviewer**: AI Assistant  
**Scope**: Reliability of block execution (`src/blocks/*.cpp`) and runtime management (`src/runtime/*`)

## Executive Summary

This review examines the reliability of block execution and runtime management in the CAF processor. Key findings:

- ✅ **Good**: Basic error handling exists in FS and HTTP blocks
- ✅ **Good**: HTTP blocks have timeout support via CURL
- ✅ **Good**: Runtime has basic backpressure via `max_concurrency_`
- ❌ **Missing**: FS operations lack timeout enforcement
- ❌ **Missing**: No retry logic at block level (only at executor actor level)
- ❌ **Missing**: No cancellation support for FS/HTTP operations
- ❌ **Missing**: No queue size limits (unbounded growth risk)
- ⚠️ **Incomplete**: Cancellation implementation has TODO comments

## 1. Filesystem Block (`src/blocks/fs_block.cpp`)

### Current State

**Error Handling**: ✅ Good
- Try-catch blocks around all FS operations
- Proper error messages returned
- Security checks for path validation

**Issues Found**:

1. **No Timeout Enforcement** ❌
   - File I/O operations can block indefinitely
   - No timeout mechanism for `std::ofstream` or `std::ifstream`
   - Large file operations can hang the thread

2. **No Retry Logic** ❌
   - Retries are handled at executor actor level, not block level
   - FS operations should have configurable retry for transient errors (disk full, permission denied)

3. **No Cancellation Support** ❌
   - `BaseBlockExecutor::cancel()` is a no-op
   - No way to interrupt long-running file operations
   - Thread will block until operation completes

4. **Error Classification** ⚠️
   - All errors are generic `std::runtime_error`
   - Should distinguish transient vs permanent errors
   - Should use `ErrorCode` enum from `core.hpp`

### Recommendations

```cpp
// Add timeout support using std::filesystem with timeout wrapper
class FsBlockExecutor : public BaseBlockExecutor {
private:
    bool execute_with_timeout(std::function<void()> op, int64_t timeout_ms) {
        std::atomic<bool> completed{false};
        std::atomic<bool> cancelled{false};
        
        // Run operation in separate thread
        std::thread worker([&]() {
            try {
                op();
                completed = true;
            } catch (...) {
                completed = true;
            }
        });
        
        // Wait with timeout
        auto start = std::chrono::steady_clock::now();
        while (!completed && !cancelled) {
            auto elapsed = std::chrono::steady_clock::now() - start;
            if (elapsed > std::chrono::milliseconds(timeout_ms)) {
                cancelled = true;
                worker.detach(); // Cannot join if blocking
                return false;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
        
        if (worker.joinable()) {
            worker.join();
        }
        return completed && !cancelled;
    }
    
    // Add cancellation token
    std::atomic<bool> cancelled_{false};
    
public:
    caf::expected<void> cancel(const std::string& step_id) override {
        cancelled_ = true;
        return caf::unit;
    }
};
```

**Action Items**:
1. Add timeout wrapper for FS operations
2. Implement cancellation token mechanism
3. Add retry logic for transient errors (with exponential backoff)
4. Classify errors using `ErrorCode` enum
5. Add file size limits to prevent memory exhaustion

## 2. HTTP Block (`src/blocks/http_block.cpp`)

### Current State

**Error Handling**: ✅ Good
- Try-catch blocks around HTTP operations
- CURL error handling

**Timeout Support**: ✅ Partial
- `CURLOPT_TIMEOUT_MS` is set
- Missing `CURLOPT_CONNECTTIMEOUT_MS` for connection timeout

**Issues Found**:

1. **Incomplete Timeout Configuration** ⚠️
   - Only total timeout set, no connection timeout
   - Should set both `CURLOPT_TIMEOUT_MS` and `CURLOPT_CONNECTTIMEOUT_MS`

2. **No Retry Logic at Block Level** ❌
   - Retries handled at executor actor level
   - Should retry on network errors (connection refused, timeout)
   - Should not retry on HTTP 4xx errors (client errors)

3. **No Cancellation Support** ❌
   - Cannot cancel in-flight HTTP requests
   - CURL operations block until completion
   - No way to interrupt long-running downloads

4. **Error Classification** ⚠️
   - All errors are generic
   - Should distinguish network errors from HTTP errors
   - Should use `ErrorCode` enum (e.g., `network_error`, `connection_timeout`, `http_error`)

5. **Missing HTTP Status Code Handling** ⚠️
   - Non-2xx status codes are treated as errors
   - Should distinguish 4xx (client error) vs 5xx (server error)
   - 5xx errors might be retryable

### Recommendations

```cpp
class HttpBlockExecutor : public BaseBlockExecutor {
private:
    // Add cancellation support
    std::atomic<bool> cancelled_{false};
    CURL* current_curl_{nullptr};
    std::mutex curl_mutex_;
    
    HttpResponse perform_http_request(const std::string& url, 
                                     const std::string& method, 
                                     const std::string& body, 
                                     const Json::Value& headers, 
                                     int64_t timeout_ms) {
        std::lock_guard<std::mutex> lock(curl_mutex_);
        
        CURL* curl = curl_easy_init();
        if (!curl) {
            throw std::runtime_error("Failed to initialize CURL");
        }
        
        current_curl_ = curl; // Store for cancellation
        
        // Set timeouts
        curl_easy_setopt(curl, CURLOPT_TIMEOUT_MS, timeout_ms);
        curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT_MS, 
                        std::min(timeout_ms / 4, 5000L)); // 25% of total or 5s max
        
        // ... rest of setup ...
        
        // Check cancellation before perform
        if (cancelled_) {
            curl_easy_cleanup(curl);
            current_curl_ = nullptr;
            throw std::runtime_error("Request cancelled");
        }
        
        CURLcode res = curl_easy_perform(curl);
        
        current_curl_ = nullptr; // Clear after completion
        
        // ... rest of cleanup ...
    }
    
public:
    caf::expected<void> cancel(const std::string& step_id) override {
        cancelled_ = true;
        std::lock_guard<std::mutex> lock(curl_mutex_);
        if (current_curl_) {
            curl_easy_setopt(current_curl_, CURLOPT_TIMEOUT_MS, 1L); // Force timeout
        }
        return caf::unit;
    }
    
    // Add retry logic for network errors
    caf::expected<StepResult> execute_with_retry(const StepRequest& req) {
        int max_retries = req.retry_count;
        int backoff_ms = 100;
        
        for (int attempt = 0; attempt <= max_retries; attempt++) {
            auto result = execute(req);
            
            if (result && result->status == StepStatus::ok) {
                result->retries_used = attempt;
                return result;
            }
            
            // Check if error is retryable
            if (result && result->error_code == ErrorCode::network_error) {
                if (attempt < max_retries) {
                    std::this_thread::sleep_for(
                        std::chrono::milliseconds(backoff_ms * (1 << attempt))
                    );
                    continue;
                }
            }
            
            // Non-retryable error or max retries reached
            if (result) {
                result->retries_used = attempt;
            }
            return result;
        }
    }
};
```

**Action Items**:
1. Add `CURLOPT_CONNECTTIMEOUT_MS` for connection timeout
2. Implement cancellation via CURL timeout manipulation
3. Add retry logic for network errors (not HTTP 4xx)
4. Classify errors using `ErrorCode` enum
5. Distinguish retryable (5xx, network) vs non-retryable (4xx) errors

## 3. Runtime Management (`src/runtime/*`)

### Current State

**Actor Pool Management**: ✅ Good
- Thread pool implementation with configurable concurrency
- Proper thread lifecycle management
- Queue-based task submission

**Backpressure**: ⚠️ Partial
- `current_load_` tracks active tasks
- `max_concurrency_` limits concurrent execution
- Tasks queued when at capacity

**Issues Found**:

1. **Unbounded Queue Growth** ❌
   - `std::queue<Task> q_` in `ActorPool` has no size limit
   - Under high load, queue can grow indefinitely
   - Risk of memory exhaustion

2. **No Queue Depth Monitoring** ⚠️
   - `queue_depth()` method exists but not used for backpressure
   - No rejection of new tasks when queue is full
   - No metrics for queue depth

3. **Incomplete Cancellation** ⚠️
   - `PoolActorState::cancel()` has TODO comment
   - No tracking of active tasks by step_id
   - Cannot cancel specific tasks in queue

4. **No Task Priority** ⚠️
   - All tasks treated equally
   - No priority queue for urgent tasks
   - No deadline-based scheduling

5. **Race Condition in Backpressure** ⚠️
   - `current_load_` increment/decrement not atomic
   - Potential for load count to go negative or exceed max

### Recommendations

```cpp
class ActorPool {
public:
    explicit ActorPool(int concurrency, size_t max_queue_size = 1000)
        : concurrency_(concurrency), 
          max_queue_size_(max_queue_size),
          stop_(false) {
        // ... thread creation ...
    }
    
    bool submit(Task t) {  // Return bool to indicate success
        std::unique_lock<std::mutex> lk(mu_);
        
        // Reject if queue is full
        if (q_.size() >= max_queue_size_) {
            return false; // Backpressure: reject new tasks
        }
        
        q_.push(std::move(t));
        cv_.notify_one();
        return true;
    }
    
    size_t queue_depth() const {
        std::unique_lock<std::mutex> lk(mu_);
        return q_.size();
    }
    
    size_t active_tasks() const {
        std::unique_lock<std::mutex> lk(mu_);
        return current_load_;
    }
    
    bool is_overloaded() const {
        std::unique_lock<std::mutex> lk(mu_);
        return q_.size() > max_queue_size_ * 0.8; // 80% threshold
    }
    
private:
    void run() {
        while (true) {
            Task task;
            {
                std::unique_lock<std::mutex> lk(mu_);
                cv_.wait(lk, [this]{ return stop_ || !q_.empty(); });
                if (stop_ && q_.empty()) return;
                task = std::move(q_.front());
                q_.pop();
                current_load_++; // Increment before releasing lock
            }
            
            try {
                task();
            } catch (...) {
                // Log error but continue processing
            }
            
            {
                std::unique_lock<std::mutex> lk(mu_);
                current_load_--; // Decrement after task completes
            }
        }
    }
    
    int concurrency_;
    size_t max_queue_size_;  // Add queue size limit
    std::atomic<int> current_load_{0};  // Make atomic
    // ... rest of members ...
};
```

**Scheduler Improvements**:

```cpp
class Scheduler {
public:
    caf::expected<caf::actor> schedule_step(const StepRequest& request, 
                                           const BlockContext& context) {
        ResourceClass resource_class = determine_resource_class(request);
        
        // Check queue depth before scheduling
        auto pool_actor = get_pool_for_resource(resource_class);
        if (!pool_actor) {
            return caf::make_error(caf::sec::runtime_error, 
                                 "No available pool for resource class");
        }
        
        // Request queue depth from pool actor
        auto depth_future = system_.request(*pool_actor, 
                                           std::chrono::seconds(1),
                                           caf::atom("queue_depth"));
        
        depth_future.await(
            [this, &request, &context](size_t depth) {
                if (depth > MAX_QUEUE_DEPTH) {
                    return caf::make_error(caf::sec::runtime_error,
                                         "Queue overloaded, rejecting request");
                }
            },
            [](caf::error err) {
                // Handle error
            }
        );
        
        return *pool_actor;
    }
    
private:
    static constexpr size_t MAX_QUEUE_DEPTH = 1000;
};
```

**Action Items**:
1. Add `max_queue_size` parameter to `ActorPool` constructor
2. Make `current_load_` atomic to prevent race conditions
3. Return `bool` from `submit()` to indicate rejection
4. Implement proper cancellation tracking by step_id
5. Add queue depth monitoring and metrics
6. Add overload detection (80% threshold)
7. Implement task priority queue (future enhancement)

## 4. Executor Actor Retry Logic (`src/worker_actor.cpp`)

### Current State

**Retry Implementation**: ✅ Good
- `execute_with_retry()` implements retry logic
- Exponential backoff (100ms * attempt)
- Tracks `retries_used` in result

**Issues Found**:

1. **Fixed Backoff** ⚠️
   - Backoff is fixed: `100 * (attempt + 1)` ms
   - Should use exponential backoff: `base * 2^attempt`
   - Should respect `RetryPolicy` from `StepRequest`

2. **No Jitter** ❌
   - Fixed backoff can cause thundering herd
   - Should add random jitter to backoff
   - Reduces synchronized retries

3. **Retries All Errors** ⚠️
   - Retries all errors, even non-retryable ones
   - Should only retry transient errors (network, timeout)
   - Should not retry permanent errors (4xx HTTP, validation)

4. **No Cancellation During Retry** ❌
   - Cannot cancel during retry wait
   - Should check cancellation flag between retries
   - Should respect timeout across all retries

### Recommendations

```cpp
caf::expected<StepResult> ExecutorActorState::execute_with_retry(const StepRequest& req) {
    StepResult final_result;
    std::atomic<bool> cancelled{false};
    
    // Store cancellation token
    cancellation_tokens_[req.ctx.step_id] = &cancelled;
    
    auto start_time = std::chrono::steady_clock::now();
    int max_retries = req.retry_count;
    int base_backoff_ms = 100;
    
    for (int attempt = 0; attempt <= max_retries; attempt++) {
        // Check cancellation
        if (cancelled) {
            final_result = StepResult::cancelled_result(req.ctx.metadata);
            break;
        }
        
        // Check total timeout
        auto elapsed = std::chrono::steady_clock::now() - start_time;
        if (elapsed > std::chrono::milliseconds(req.timeout_ms)) {
            final_result = StepResult::timeout_result(req.ctx.metadata);
            break;
        }
        
        auto result = execute_single_attempt(req);
        if (result) {
            final_result = *result;
            final_result.retries_used = attempt;
            
            // Check if success or non-retryable error
            if (final_result.status == StepStatus::ok) {
                break;
            }
            
            // Check if error is retryable
            if (!is_retryable_error(final_result.error_code)) {
                break; // Don't retry permanent errors
            }
        }
        
        // If not the last attempt, wait with exponential backoff + jitter
        if (attempt < max_retries) {
            int backoff_ms = base_backoff_ms * (1 << attempt); // Exponential
            int jitter = std::rand() % (backoff_ms / 4); // 25% jitter
            int total_wait = backoff_ms + jitter;
            
            // Wait with cancellation check
            auto wait_start = std::chrono::steady_clock::now();
            while (std::chrono::steady_clock::now() - wait_start < 
                   std::chrono::milliseconds(total_wait)) {
                if (cancelled) {
                    final_result = StepResult::cancelled_result(req.ctx.metadata);
                    return final_result;
                }
                std::this_thread::sleep_for(std::chrono::milliseconds(10));
            }
        }
    }
    
    cancellation_tokens_.erase(req.ctx.step_id);
    return final_result;
}

private:
    bool is_retryable_error(ErrorCode code) {
        switch (code) {
            case ErrorCode::network_error:
            case ErrorCode::connection_timeout:
            case ErrorCode::system_overload:
                return true;
            case ErrorCode::invalid_input:
            case ErrorCode::missing_required_field:
            case ErrorCode::permission_denied:
            case ErrorCode::quota_exceeded:
                return false;
            default:
                return false; // Conservative: don't retry unknown errors
        }
    }
    
    std::unordered_map<std::string, std::atomic<bool>*> cancellation_tokens_;
};
```

**Action Items**:
1. Implement exponential backoff with jitter
2. Add cancellation check between retries
3. Add total timeout check across all retries
4. Implement `is_retryable_error()` logic
5. Respect `RetryPolicy` from `StepRequest` (if available)

## 5. Summary of Critical Issues

### High Priority

1. **Unbounded Queue Growth** (`actor_pools.hpp`)
   - Risk: Memory exhaustion under load
   - Fix: Add `max_queue_size` and reject when full

2. **No FS Operation Timeouts** (`fs_block.cpp`)
   - Risk: Thread blocking indefinitely
   - Fix: Add timeout wrapper for file operations

3. **No Cancellation for FS/HTTP** (`fs_block.cpp`, `http_block.cpp`)
   - Risk: Cannot interrupt long-running operations
   - Fix: Implement cancellation tokens

4. **Race Condition in Backpressure** (`actor_pools.hpp`)
   - Risk: Incorrect load counting
   - Fix: Make `current_load_` atomic

### Medium Priority

5. **Incomplete HTTP Timeout** (`http_block.cpp`)
   - Fix: Add `CURLOPT_CONNECTTIMEOUT_MS`

6. **No Error Classification** (`fs_block.cpp`, `http_block.cpp`)
   - Fix: Use `ErrorCode` enum from `core.hpp`

7. **Retry All Errors** (`worker_actor.cpp`)
   - Fix: Only retry transient errors

8. **Fixed Backoff** (`worker_actor.cpp`)
   - Fix: Exponential backoff with jitter

### Low Priority

9. **No Task Priority** (`actor_pools.hpp`)
   - Enhancement: Priority queue for urgent tasks

10. **No Queue Depth Monitoring** (`scheduler.cpp`)
    - Enhancement: Metrics and alerting

## 6. Testing Recommendations

1. **Load Testing**:
   - Test queue behavior under high load
   - Verify queue rejection when full
   - Measure memory usage with unbounded queue

2. **Timeout Testing**:
   - Test FS operations with slow disk
   - Test HTTP operations with slow network
   - Verify timeout enforcement

3. **Cancellation Testing**:
   - Test cancellation during FS operations
   - Test cancellation during HTTP requests
   - Test cancellation during retry wait

4. **Error Handling Testing**:
   - Test retry logic with transient errors
   - Test no retry for permanent errors
   - Test error classification

5. **Backpressure Testing**:
   - Test queue depth limits
   - Test rejection when overloaded
   - Test metrics collection

## 7. Implementation Priority

**Phase 1 (Critical)**:
1. Add queue size limits to `ActorPool`
2. Make `current_load_` atomic
3. Add timeout support for FS operations
4. Implement cancellation tokens

**Phase 2 (Important)**:
5. Add connection timeout to HTTP
6. Implement error classification
7. Fix retry logic (exponential backoff, error filtering)

**Phase 3 (Enhancements)**:
8. Add task priority queue
9. Add queue depth metrics
10. Add overload detection and alerting

## References

- `apps/caf/processor/src/blocks/fs_block.cpp` - Filesystem block implementation
- `apps/caf/processor/src/blocks/http_block.cpp` - HTTP block implementation
- `apps/caf/processor/src/runtime/actor_pools.hpp` - Actor pool implementation
- `apps/caf/processor/src/runtime/scheduler.cpp` - Scheduler implementation
- `apps/caf/processor/src/worker_actor.cpp` - Executor actor with retry logic
- `apps/caf/processor/include/beamline/worker/core.hpp` - Core types and interfaces


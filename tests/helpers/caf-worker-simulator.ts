import { EventEmitter } from 'events';
import { randomUUID } from 'crypto';

export interface TaskRequest {
  id: string;
  type: string;
  payload: any;
  timeout: number;
  retryCount: number;
  tenantId: string;
  traceId: string;
}

export interface TaskResult {
  id: string;
  status: 'success' | 'error' | 'timeout';
  result?: any;
  error?: string;
  executionTime: number;
  workerId: string;
}

export interface WorkerMetrics {
  tasksProcessed: number;
  tasksFailed: number;
  averageExecutionTime: number;
  currentLoad: number;
  maxLoad: number;
}

export class CAFWorkerSimulator extends EventEmitter {
  private workerId: string;
  private isRunning: boolean = false;
  private tasks: Map<string, TaskRequest> = new Map();
  private metrics: WorkerMetrics;
  private processingDelay: number = 100; // ms
  private failureRate: number = 0.05; // 5% failure rate

  constructor(workerId?: string) {
    super();
    this.workerId = workerId || `worker-${randomUUID().slice(0, 8)}`;
    this.metrics = {
      tasksProcessed: 0,
      tasksFailed: 0,
      averageExecutionTime: 0,
      currentLoad: 0,
      maxLoad: 100
    };
  }

  async start(): Promise<void> {
    if (this.isRunning) {
      throw new Error(`Worker ${this.workerId} is already running`);
    }
    
    this.isRunning = true;
    this.emit('worker:started', { workerId: this.workerId });
    
    // Simulate worker heartbeat
    this.startHeartbeat();
  }

  async stop(): Promise<void> {
    if (!this.isRunning) {
      return;
    }
    
    this.isRunning = false;
    this.emit('worker:stopped', { workerId: this.workerId });
    
    // Clear any pending tasks
    this.tasks.clear();
  }

  async processTask(task: TaskRequest): Promise<TaskResult> {
    if (!this.isRunning) {
      throw new Error(`Worker ${this.workerId} is not running`);
    }

    const startTime = Date.now();
    this.tasks.set(task.id, task);
    this.metrics.currentLoad++;

    try {
      // Simulate task processing
      await this.simulateProcessing(task);
      
      // Simulate random failures
      if (Math.random() < this.failureRate) {
        throw new Error('Simulated worker failure');
      }

      const result = await this.executeTask(task);
      const executionTime = Date.now() - startTime;
      
      this.updateMetrics(executionTime, true);
      this.tasks.delete(task.id);
      this.metrics.currentLoad--;

      const taskResult: TaskResult = {
        id: task.id,
        status: 'success',
        result,
        executionTime,
        workerId: this.workerId
      };

      this.emit('task:completed', taskResult);
      return taskResult;

    } catch (error) {
      const executionTime = Date.now() - startTime;
      this.updateMetrics(executionTime, false);
      this.tasks.delete(task.id);
      this.metrics.currentLoad--;

      const taskResult: TaskResult = {
        id: task.id,
        status: 'error',
        error: error instanceof Error ? error.message : 'Unknown error',
        executionTime,
        workerId: this.workerId
      };

      this.emit('task:failed', taskResult);
      return taskResult;
    }
  }

  private async simulateProcessing(task: TaskRequest): Promise<void> {
    // Simulate variable processing time based on task complexity
    const baseDelay = this.processingDelay;
    const complexityDelay = task.payload?.complexity || 0;
    const totalDelay = baseDelay + complexityDelay + Math.random() * 50;
    
    await new Promise(resolve => setTimeout(resolve, totalDelay));
  }

  private async executeTask(task: TaskRequest): Promise<any> {
    // Simulate different task types
    switch (task.type) {
      case 'text.completion':
        return this.simulateTextCompletion(task.payload);
      
      case 'embedding':
        return this.simulateEmbedding(task.payload);
      
      case 'image.generation':
        return this.simulateImageGeneration(task.payload);
      
      case 'code.execution':
        return this.simulateCodeExecution(task.payload);
      
      default:
        return { processed: true, type: task.type, payload: task.payload };
    }
  }

  private simulateTextCompletion(payload: any): any {
    return {
      text: `Generated completion for: ${payload.prompt?.slice(0, 50) || 'unknown prompt'}`,
      tokens: Math.floor(Math.random() * 100) + 50,
      model: payload.model || 'default-model'
    };
  }

  private simulateEmbedding(payload: any): any {
    const dimensions = payload.dimensions || 768;
    return {
      embedding: Array.from({ length: dimensions }, () => Math.random() * 2 - 1),
      dimensions,
      model: payload.model || 'embedding-model'
    };
  }

  private simulateImageGeneration(payload: any): any {
    return {
      url: `https://generated-image.example.com/${randomUUID()}.png`,
      width: payload.width || 1024,
      height: payload.height || 1024,
      model: payload.model || 'image-model'
    };
  }

  private simulateCodeExecution(payload: any): any {
    return {
      output: `Executed code: ${payload.code?.slice(0, 30) || 'unknown code'}`,
      exitCode: 0,
      executionTime: Math.random() * 1000
    };
  }

  private updateMetrics(executionTime: number, success: boolean): void {
    this.metrics.tasksProcessed++;
    if (!success) {
      this.metrics.tasksFailed++;
    }
    
    // Update average execution time
    const totalTasks = this.metrics.tasksProcessed;
    this.metrics.averageExecutionTime = 
      (this.metrics.averageExecutionTime * (totalTasks - 1) + executionTime) / totalTasks;
  }

  private startHeartbeat(): void {
    const heartbeatInterval = setInterval(() => {
      if (!this.isRunning) {
        clearInterval(heartbeatInterval);
        return;
      }

      this.emit('worker:heartbeat', {
        workerId: this.workerId,
        metrics: { ...this.metrics },
        timestamp: Date.now()
      });
    }, 5000); // 5 second heartbeat
  }

  getMetrics(): WorkerMetrics {
    return { ...this.metrics };
  }

  getWorkerId(): string {
    return this.workerId;
  }

  isHealthy(): boolean {
    return this.isRunning && this.metrics.currentLoad < this.metrics.maxLoad;
  }

  getActiveTasks(): TaskRequest[] {
    return Array.from(this.tasks.values());
  }

  setProcessingDelay(delay: number): void {
    this.processingDelay = Math.max(0, delay);
  }

  setFailureRate(rate: number): void {
    this.failureRate = Math.max(0, Math.min(1, rate));
  }

  simulateOverload(): void {
    this.metrics.currentLoad = this.metrics.maxLoad + 20;
    this.emit('worker:overload', {
      workerId: this.workerId,
      currentLoad: this.metrics.currentLoad,
      maxLoad: this.metrics.maxLoad
    });
  }

  simulateRecovery(): void {
    this.metrics.currentLoad = Math.floor(this.metrics.maxLoad * 0.3);
    this.emit('worker:recovered', {
      workerId: this.workerId,
      currentLoad: this.metrics.currentLoad
    });
  }
}
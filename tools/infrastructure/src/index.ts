#!/usr/bin/env node
import { MCPServerWrapper } from "./mcp-server-wrapper.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "./script-runner.js";

class InfrastructureServer {
  private server: MCPServerWrapper;
  private runner: ScriptRunner;
  constructor() {
    this.server = new MCPServerWrapper({ name: "infrastructure-mcp", version: "1.0.0" }, { capabilities: { tools: {} } });
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }
  private setupHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        { name: "validate_infra", description: "Validate infrastructure configuration files including Docker Compose, service definitions, and environment variables. Use this tool to ensure all infrastructure configuration is correct before starting services. Checks file presence, syntax validity, and required configuration values. Returns validation report with any configuration errors.", inputSchema: { type: "object", properties: {} } },
        { name: "verify_services", description: "Verify that all required services are running and healthy. Use this tool to check service availability and health status after starting infrastructure. Checks HTTP health endpoints, database connectivity, and service dependencies. Returns service status report with any unavailable or unhealthy services.", inputSchema: { type: "object", properties: {} } },
        { name: "init_db", description: "Initialize database schema with required tables and indexes. Use this tool to set up database structure for local development or testing. Creates all necessary tables, indexes, and initial data. Returns initialization status and schema version information.", inputSchema: { type: "object", properties: {} } },
        { name: "local_up", description: "Start local development services using Docker Compose. Use this tool to bring up the complete local development environment including databases, message brokers, and application services. Starts all services defined in docker-compose.yml. Returns startup status for each service.", inputSchema: { type: "object", properties: {} } },
        { name: "local_down", description: "Stop local development services using Docker Compose. Use this tool to gracefully shut down all local development services and release resources. Stops all containers and removes networks. Returns shutdown status for each service.", inputSchema: { type: "object", properties: {} } },
        { name: "local_build", description: "Build local development service Docker images. Use this tool to rebuild Docker images after code changes or configuration updates. Builds images for all services defined in docker-compose.yml. Returns build status and image tags.", inputSchema: { type: "object", properties: {} } },
        { name: "local_logs", description: "View logs from local development services. Use this tool to inspect service logs for debugging or monitoring. Optionally filter logs for a specific service. Returns recent log entries with timestamps and log levels.", inputSchema: { type: "object", properties: { service: { type: "string" } } } },
        { name: "setup_local", description: "Set up complete local development environment including dependencies, configuration, and initial data. Use this tool for initial environment setup or to reset environment to clean state. Installs dependencies, creates configuration files, and initializes databases. Returns setup status and next steps.", inputSchema: { type: "object", properties: {} } },
        { name: "validate_env", description: "Validate environment configuration including required environment variables, file permissions, and external dependencies. Use this tool to ensure local environment is properly configured before starting development. Checks environment variables, file paths, and dependency availability. Returns validation report with any missing or incorrect configuration.", inputSchema: { type: "object", properties: {} } },
      ],
    }));
    this.server.setRequestHandler(CallToolRequestSchema, async (request: any) => {
      const { name, arguments: args } = request.params;
      const scripts: Record<string, string> = {
        validate_infra: "scripts/infra/validate_infra.sh",
        verify_services: "scripts/infra/verify-services.sh",
        init_db: "scripts/infra/init-db.sh",
        local_up: "scripts/local/up.sh",
        local_down: "scripts/local/down.sh",
        local_build: "scripts/local/build.sh",
        local_logs: "scripts/local/logs.sh",
        setup_local: "scripts/setup-local.sh",
        validate_env: "scripts/validate-env.sh",
      };
      if (!scripts[name]) throw new Error(`Unknown tool: ${name}`);
      const serviceArg = name === "local_logs" && args?.service ? [(args.service as string)] : [];
      const result = await this.runner.runScript(scripts[name], {}, ...serviceArg);
      return { content: [{ type: "text", text: result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "") }] };
    });
  }
  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("Infrastructure MCP Server running");
  }
}
new InfrastructureServer().run().catch(console.error);


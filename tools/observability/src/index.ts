#!/usr/bin/env node
import { MCPServerWrapper } from "./mcp-server-wrapper.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "./script-runner.js";

class ObservabilityServer {
  private server: MCPServerWrapper;
  private runner: ScriptRunner;
  constructor() {
    this.server = new MCPServerWrapper({ name: "observability-mcp", version: "1.0.0" }, { capabilities: { tools: {} } });
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }
  private setupHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        { name: "validate_observability", description: "Validate observability configuration including JSON log format schema and health endpoint specifications. Use this tool to ensure observability configuration files are present, properly structured, and meet MVP requirements. Checks log format schema, health endpoint definitions, and validation script availability. Returns validation report with configuration status.", inputSchema: { type: "object", properties: {} } },
        {
          name: "run_obs1_validators",
          description: "Run OBS-1 (Observability Standard 1) validators to check JSON log conformance. Use this tool to validate that application logs conform to OBS-1 standard with required fields (timestamp, level, msg, trace_id, tenant_id). Optionally collect validation results or validate specific log file paths. Returns validation report with conformance status.",
          inputSchema: {
            type: "object",
            properties: {
              collect: { type: "boolean", default: false },
              paths: { type: "array", items: { type: "string" } },
            },
          },
        },
        {
          name: "obs1_jq_validate",
          description: "Validate JSON log file conformance using jq tool. Use this tool to check that a specific log file contains valid JSON entries conforming to OBS-1 standard. Validates required fields, data types, and JSON structure. Requires log_file parameter. Returns validation results with any non-conforming entries.",
          inputSchema: { type: "object", properties: { log_file: { type: "string" } }, required: ["log_file"] },
        },
        {
          name: "obs1_node_validate",
          description: "Validate JSON log file conformance using Node.js. Use this tool to check that a specific log file contains valid JSON entries conforming to OBS-1 standard. Uses Node.js JSON parser for validation with detailed error reporting. Requires log_file parameter. Returns validation results with parsing errors if any.",
          inputSchema: { type: "object", properties: { log_file: { type: "string" } }, required: ["log_file"] },
        },
      ],
    }));
    this.server.setRequestHandler(CallToolRequestSchema, async (request: any) => {
      const { name, arguments: args } = request.params;
      let result;
      if (name === "validate_observability") {
        result = await this.runner.runScript("scripts/observability/validate_observability.sh");
      } else if (name === "run_obs1_validators") {
        const scriptArgs: string[] = [];
        if (args?.collect as boolean | undefined) scriptArgs.push("--collect");
        const paths = (args?.paths as string[] | undefined);
        if (paths) scriptArgs.push("--paths", ...paths);
        result = await this.runner.runScript("scripts/observability/run_obs1_validators.sh", {}, ...scriptArgs);
      } else if (name === "obs1_jq_validate") {
        const logFile = args?.log_file as string | undefined;
        if (!logFile) throw new Error("log_file required");
        result = await this.runner.runScript("scripts/obs1_jq_validate.sh", {}, logFile);
      } else if (name === "obs1_node_validate") {
        const logFile = args?.log_file as string | undefined;
        if (!logFile) throw new Error("log_file required");
        result = await this.runner.runNodeScript("scripts/obs1_node_validate.mjs", [logFile]);
      } else throw new Error(`Unknown tool: ${name}`);
      return { content: [{ type: "text", text: result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "") }] };
    });
  }
  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("Observability MCP Server running");
  }
}
new ObservabilityServer().run().catch(console.error);

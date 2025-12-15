#!/usr/bin/env node
import { MCPServerWrapper } from "./mcp-server-wrapper.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "./script-runner.js";

class CPValidationServer {
  private server: MCPServerWrapper;
  private runner: ScriptRunner;
  constructor() {
    this.server = new MCPServerWrapper({ name: "cp-validation-mcp", version: "1.0.0" }, { capabilities: { tools: {} } });
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }
  private setupHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        { name: "check_cp1_contracts", description: "Verify CP1 (Checkpoint 1) contracts compliance across all components. Use this tool to validate that all components meet CP1 contract requirements, including API contracts, data structures, and integration points. This is a critical validation step before marking CP1 as complete. Returns detailed contract validation report with any violations found.", inputSchema: { type: "object", properties: {} } },
        { name: "check_cp2_implementation", description: "Check CP2 (Checkpoint 2) implementation completeness and correctness. Use this tool to verify that CP2 features are fully implemented according to specifications. Validates code structure, API implementations, and feature completeness. Returns implementation status report with missing or incomplete features.", inputSchema: { type: "object", properties: {} } },
        { name: "check_cp2_behavior", description: "Verify CP2 (Checkpoint 2) behavior matches expected specifications. Use this tool to validate that CP2 features behave correctly according to requirements. Tests functional behavior, edge cases, and integration scenarios. Returns behavior validation report with any discrepancies found.", inputSchema: { type: "object", properties: {} } },
        { name: "validate_all_projects", description: "Validate all sub-projects for CP1 contract compliance. Use this tool to perform comprehensive CP1 validation across all project components and sub-projects. This ensures consistent CP1 compliance across the entire codebase. Returns aggregated validation report for all projects.", inputSchema: { type: "object", properties: {} } },
      ],
    }));
    this.server.setRequestHandler(CallToolRequestSchema, async (request: any) => {
      const { name } = request.params;
      const scripts: Record<string, string> = {
        check_cp1_contracts: "scripts/check_cp1_contracts.sh",
        check_cp2_implementation: "scripts/check_cp2_implementation.sh",
        check_cp2_behavior: "scripts/check_cp2_behavior.sh",
        validate_all_projects: "scripts/validate_all_projects.sh",
      };
      if (!scripts[name]) throw new Error(`Unknown tool: ${name}`);
      const result = await this.runner.runScript(scripts[name]);
      return { content: [{ type: "text", text: result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "") }] };
    });
  }
  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("CP Validation MCP Server running");
  }
}
new CPValidationServer().run().catch(console.error);


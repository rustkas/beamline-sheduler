#!/usr/bin/env node
import { MCPServerWrapper } from "./mcp-server-wrapper.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "./script-runner.js";

class CIValidationServer {
  private server: MCPServerWrapper;
  private runner: ScriptRunner;
  constructor() {
    this.server = new MCPServerWrapper({ name: "ci-validation-mcp", version: "1.0.0" }, { capabilities: { tools: {} } });
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }
  private setupHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        {
          name: "dry_run_ci",
          description: "Run comprehensive CI/CD validation gates locally to test changes before pushing to remote. Use this tool to simulate the full CI/CD pipeline validation locally, including schema validation, HMAC chain verification, security checks, backend/frontend tests, QA validation, and compliance checks. Optionally run specific validation step or all steps (default). Returns detailed validation report matching CI/CD output format.",
          inputSchema: {
            type: "object",
            properties: {
              step: {
                type: "string",
                enum: ["schema", "hmac", "security", "backend", "frontend", "qa", "compliance", "summary", "all"],
                default: "all",
              },
            },
          },
        },
        { name: "run_checks", description: "Run quick local validation checks for common issues. Use this tool for fast pre-commit validation without running the full CI/CD pipeline. Performs essential checks like syntax validation, basic security checks, and quick smoke tests. Returns summary of quick check results.", inputSchema: { type: "object", properties: {} } },
      ],
    }));
    this.server.setRequestHandler(CallToolRequestSchema, async (request: any) => {
      const { name, arguments: args } = request.params;
      if (name === "dry_run_ci") {
        const step = args?.step as string | undefined || "all";
        const result = await this.runner.runScript("scripts/dry_run_ci.sh", {}, step);
        return { content: [{ type: "text", text: result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "") }] };
      } else if (name === "run_checks") {
        const result = await this.runner.runScript("scripts/run_checks.sh");
        return { content: [{ type: "text", text: result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "") }] };
      }
      throw new Error(`Unknown tool: ${name}`);
    });
  }
  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("CI/CD Validation MCP Server running");
  }
}
new CIValidationServer().run().catch(console.error);


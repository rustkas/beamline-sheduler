#!/usr/bin/env node
import { MCPServerWrapper } from "./mcp-server-wrapper.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "./script-runner.js";

class ComplianceServer {
  private server: MCPServerWrapper;
  private runner: ScriptRunner;
  constructor() {
    this.server = new MCPServerWrapper({ name: "compliance-mcp", version: "1.0.0" }, { capabilities: { tools: {} } });
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }
  private setupHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        { name: "validate_compliance", description: "Validate compliance artifacts including license registry, privacy policy, and SBOM templates. Use this tool to ensure all compliance documentation is present, properly structured, and contains required information. Checks file presence, JSON schema validation, and required field completeness. Returns compliance validation report with any missing or invalid artifacts.", inputSchema: { type: "object", properties: {} } },
        { name: "check_license_compliance", description: "Check license compliance for all project dependencies and components. Use this tool to verify that all dependencies use licenses compatible with project license policy. Validates license registry allowlist/denylist and checks for license conflicts. Returns license compliance report with any violations or conflicts found.", inputSchema: { type: "object", properties: {} } },
      ],
    }));
    this.server.setRequestHandler(CallToolRequestSchema, async (request: any) => {
      const { name } = request.params;
      const script = name === "validate_compliance" ? "scripts/compliance/validate_compliance.sh" : "scripts/check_license_compliance.sh";
      const result = await this.runner.runScript(script);
      return { content: [{ type: "text", text: result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "") }] };
    });
  }
  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("Compliance MCP Server running");
  }
}
new ComplianceServer().run().catch(console.error);

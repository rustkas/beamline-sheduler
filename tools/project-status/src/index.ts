#!/usr/bin/env node
import { MCPServerWrapper } from "./mcp-server-wrapper.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "./script-runner.js";

class ProjectStatusServer {
  private server: MCPServerWrapper;
  private runner: ScriptRunner;
  constructor() {
    this.server = new MCPServerWrapper({ name: "project-status-mcp", version: "1.0.0" }, { capabilities: { tools: {} } });
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }
  private setupHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [{ name: "generate_project_status", description: "Generate comprehensive project status report including component status, checkpoint progress, worker status, and artifact checksums. Use this tool to get an overview of project state, track progress across checkpoints, and identify any blocked or incomplete work. Returns detailed status report with component-by-component breakdown.", inputSchema: { type: "object", properties: {} } }],
    }));
    this.server.setRequestHandler(CallToolRequestSchema, async (request: any) => {
      const { name } = request.params;
      if (name !== "generate_project_status") throw new Error(`Unknown tool: ${name}`);
      const result = await this.runner.runScript("scripts/generate_project_status.sh");
      return { content: [{ type: "text", text: result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "") }] };
    });
  }
  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("Project Status MCP Server running");
  }
}
new ProjectStatusServer().run().catch(console.error);


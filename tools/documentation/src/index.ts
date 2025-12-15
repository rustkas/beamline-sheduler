#!/usr/bin/env node
import { MCPServerWrapper } from "./mcp-server-wrapper.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "./script-runner.js";

class DocumentationServer {
  private server: MCPServerWrapper;
  private runner: ScriptRunner;
  constructor() {
    this.server = new MCPServerWrapper({ name: "documentation-mcp", version: "1.0.0" }, { capabilities: { tools: {} } });
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }
  private setupHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [{
        name: "check_links",
        description: "Validate all links in documentation files to ensure they are not broken. Use this tool to check internal links (pointing to files in repository) and external links (HTTP/HTTPS URLs) for accessibility. Optionally specify directory to check (default: docs). Returns validation report with broken links and their locations.",
        inputSchema: { type: "object", properties: { directory: { type: "string", default: "docs" } } },
      }],
    }));
    this.server.setRequestHandler(CallToolRequestSchema, async (request: any) => {
      const { name, arguments: args } = request.params;
      if (name !== "check_links") throw new Error(`Unknown tool: ${name}`);
      const dir = args?.directory as string | undefined || "docs";
      const result = await this.runner.runScript("scripts/check_links.sh", {}, dir);
      return { content: [{ type: "text", text: result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "") }] };
    });
  }
  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("Documentation MCP Server running");
  }
}
new DocumentationServer().run().catch(console.error);

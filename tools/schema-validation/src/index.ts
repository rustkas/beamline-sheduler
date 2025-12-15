#!/usr/bin/env node
import { MCPServerWrapper } from "./mcp-server-wrapper.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "./script-runner.js";

class SchemaValidationServer {
  private server: MCPServerWrapper;
  private runner: ScriptRunner;

  constructor() {
    this.server = new MCPServerWrapper({ name: "schema-validation-mcp", version: "1.0.0" }, { capabilities: { tools: {} } });
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }

  private setupHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [{
        name: "check_schema_changes",
        description: "Validate schema version consistency between .trae/manifest.json and actual schema files (STATE.schema.json, HISTORY.schema.json). Use this tool to ensure that schema versions declared in manifest match the actual schema file versions. This prevents version mismatches that could cause validation failures. Returns validation report with version comparisons and any inconsistencies found.",
        inputSchema: { type: "object", properties: {} },
      }],
    }));

    this.server.setRequestHandler(CallToolRequestSchema, async (request: any) => {
      const { name } = request.params;
      if (name !== "check_schema_changes") throw new Error(`Unknown tool: ${name}`);
      const result = await this.runner.runScript("scripts/check_schema_changes.sh");
      return { content: [{ type: "text", text: result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "") }] };
    });
  }

  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("Schema Validation MCP Server running");
  }
}

new SchemaValidationServer().run().catch(console.error);


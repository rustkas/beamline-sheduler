#!/usr/bin/env node
import { MCPServerWrapper } from "./mcp-server-wrapper.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "./script-runner.js";

class SecurityServer {
  private server: MCPServerWrapper;
  private runner: ScriptRunner;

  constructor() {
    this.server = new MCPServerWrapper({ name: "security-mcp", version: "1.0.0" }, { capabilities: { tools: {} } });
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }

  private setupHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        {
          name: "check_hmac_masking",
          description: "Check that HMAC values in documentation are properly masked (first 16 hex chars + '...'). Use this tool to ensure no real HMAC secrets are exposed in documentation files. Scans documentation for unmasked HMAC values and reports violations. Optionally check specific files or all documentation (default). Returns list of files with unmasked HMAC values.",
          inputSchema: {
            type: "object",
            properties: {
              files: { type: "array", items: { type: "string" }, description: "Files to check (default: all docs)" },
            },
          },
        },
        { name: "check_secret_leaks", description: "Detect potential secret leaks in repository code and configuration files. Use this tool to scan for exposed secrets, API keys, passwords, and other sensitive information that should not be committed to version control. Uses pattern matching to detect common secret formats. Returns list of potential secret leaks with file locations.", inputSchema: { type: "object", properties: {} } },
        {
          name: "check_secret_compatibility",
          description: "Check if a secret key is compatible with existing HMAC chain in history.json. Use this tool before rotating HMAC secrets to verify that the new secret can be used without breaking the existing HMAC chain. Validates secret format and checks compatibility with current chain. Returns compatibility status and migration recommendations if needed.",
          inputSchema: {
            type: "object",
            properties: {
              secret: { type: "string", description: "Secret to check" },
              verbose: { type: "boolean", default: false },
            },
            required: ["secret"],
          },
        },
      ],
    }));

    this.server.setRequestHandler(CallToolRequestSchema, async (request: any) => {
      const { name, arguments: args } = request.params;
      let result;
      if (name === "check_hmac_masking") {
        result = await this.runner.runScript("scripts/check_hmac_masking.sh", {}, ...((args?.files as string[] | undefined) || []));
      } else if (name === "check_secret_leaks") {
        result = await this.runner.runScript("scripts/check_secret_leaks.sh");
      } else if (name === "check_secret_compatibility") {
        const secret = args?.secret as string | undefined;
        if (!secret) throw new Error("Secret parameter is required");
        const pyArgs = ["--secret", secret];
        if (args?.verbose as boolean | undefined) pyArgs.push("--verbose");
        result = await this.runner.runPythonScript("scripts/check_secret_compatibility.py", pyArgs);
      } else {
        throw new Error(`Unknown tool: ${name}`);
      }
      return { content: [{ type: "text", text: result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "") }] };
    });
  }

  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("Security MCP Server running");
  }
}

new SecurityServer().run().catch(console.error);


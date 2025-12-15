#!/usr/bin/env node
import { MCPServerWrapper } from "./mcp-server-wrapper.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "./script-runner.js";

class ReleaseManagementServer {
  private server: MCPServerWrapper;
  private runner: ScriptRunner;
  constructor() {
    this.server = new MCPServerWrapper({ name: "release-management-mcp", version: "1.0.0" }, { capabilities: { tools: {} } });
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }
  private setupHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        { name: "check_version_gates", description: "Check version gates before release to ensure all version requirements are met. Use this tool to validate SemVer format, version consistency across artifacts, state validation (no_drift flag), and documentation completeness (CHANGELOG.md, release notes). Optionally specify version to check or check current version. Returns gate validation report with any failures.", inputSchema: { type: "object", properties: { version: { type: "string" } } } },
        { name: "simulate_release", description: "Simulate complete release process without actually creating a release. Use this tool to test release procedures, validate all release gates, and verify release artifacts before actual release. Requires version parameter. Performs all release checks in dry-run mode. Returns simulation report with release readiness status.", inputSchema: { type: "object", properties: { version: { type: "string" } }, required: ["version"] } },
      ],
    }));
    this.server.setRequestHandler(CallToolRequestSchema, async (request: any) => {
      const { name, arguments: args } = request.params;
      if (name === "check_version_gates") {
        const version = args?.version as string | undefined;
        const versionArgs = version ? [version] : [];
        const result = await this.runner.runScript("scripts/check_version_gates.sh", {}, ...versionArgs);
        return { content: [{ type: "text", text: result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "") }] };
      } else if (name === "simulate_release") {
        const version = args?.version as string | undefined;
        if (!version) throw new Error("Version parameter is required");
        const result = await this.runner.runScript("scripts/simulate_release.sh", {}, version);
        return { content: [{ type: "text", text: result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "") }] };
      }
      throw new Error(`Unknown tool: ${name}`);
    });
  }
  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("Release Management MCP Server running");
  }
}
new ReleaseManagementServer().run().catch(console.error);


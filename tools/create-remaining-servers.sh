#!/bin/bash
# Создание оставшихся MCP серверов

cat > compliance/src/index.ts << 'EOF'
#!/usr/bin/env node
import { Server } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "../../shared/script-runner.js";

class ComplianceServer {
  private server: Server;
  private runner: ScriptRunner;
  constructor() {
    this.server = new Server({ name: "compliance-mcp", version: "1.0.0" }, { capabilities: { tools: {} } });
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }
  private setupHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        { name: "validate_compliance", description: "Validate compliance artifacts", inputSchema: { type: "object", properties: {} } },
        { name: "check_license_compliance", description: "Check license compliance", inputSchema: { type: "object", properties: {} } },
      ],
    }));
    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
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
EOF

cat > observability/src/index.ts << 'EOF'
#!/usr/bin/env node
import { Server } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "../../shared/script-runner.js";

class ObservabilityServer {
  private server: Server;
  private runner: ScriptRunner;
  constructor() {
    this.server = new Server({ name: "observability-mcp", version: "1.0.0" }, { capabilities: { tools: {} } });
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }
  private setupHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        { name: "validate_observability", description: "Validate observability configuration", inputSchema: { type: "object", properties: {} } },
        {
          name: "run_obs1_validators",
          description: "Run OBS-1 validators",
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
          description: "Validate JSON logs with jq",
          inputSchema: { type: "object", properties: { log_file: { type: "string" } }, required: ["log_file"] },
        },
        {
          name: "obs1_node_validate",
          description: "Validate JSON logs with Node",
          inputSchema: { type: "object", properties: { log_file: { type: "string" } }, required: ["log_file"] },
        },
      ],
    }));
    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;
      let result;
      if (name === "validate_observability") {
        result = await this.runner.runScript("scripts/observability/validate_observability.sh");
      } else if (name === "run_obs1_validators") {
        const scriptArgs: string[] = [];
        if (args?.collect) scriptArgs.push("--collect");
        if (args?.paths) scriptArgs.push("--paths", ...args.paths);
        result = await this.runner.runScript("scripts/observability/run_obs1_validators.sh", {}, ...scriptArgs);
      } else if (name === "obs1_jq_validate") {
        if (!args?.log_file) throw new Error("log_file required");
        result = await this.runner.runScript("scripts/obs1_jq_validate.sh", {}, args.log_file);
      } else if (name === "obs1_node_validate") {
        if (!args?.log_file) throw new Error("log_file required");
        result = await this.runner.runNodeScript("scripts/obs1_node_validate.mjs", [args.log_file]);
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
EOF

cat > documentation/src/index.ts << 'EOF'
#!/usr/bin/env node
import { Server } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "../../shared/script-runner.js";

class DocumentationServer {
  private server: Server;
  private runner: ScriptRunner;
  constructor() {
    this.server = new Server({ name: "documentation-mcp", version: "1.0.0" }, { capabilities: { tools: {} } });
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }
  private setupHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [{
        name: "check_links",
        description: "Validate links in documentation",
        inputSchema: { type: "object", properties: { directory: { type: "string", default: "docs" } } },
      }],
    }));
    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;
      if (name !== "check_links") throw new Error(`Unknown tool: ${name}`);
      const dir = args?.directory || "docs";
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
EOF

echo "Created compliance, observability, documentation servers"

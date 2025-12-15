#!/usr/bin/env node
import { MCPServerWrapper } from "./mcp-server-wrapper.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "./script-runner.js";

class ProtobufServer {
  private server: MCPServerWrapper;
  private runner: ScriptRunner;

  constructor() {
    this.server = new MCPServerWrapper({ name: "protobuf-mcp", version: "1.0.0" }, { capabilities: { tools: {} } });
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }

  private setupHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        { name: "check_proto", description: "Validate protobuf files using buf lint and buf build. Use this tool to check that all .proto files are syntactically correct and compile successfully. This validates proto file syntax, imports, and basic compatibility. Returns validation report with any errors or warnings found in proto files.", inputSchema: { type: "object", properties: {} } },
        { name: "generate_proto_stubs", description: "Generate protobuf code stubs for Erlang and TypeScript from .proto files. Use this tool after modifying proto definitions to regenerate client/server code. Generates Erlang modules and TypeScript interfaces based on proto message and service definitions. Returns generation status and list of generated files.", inputSchema: { type: "object", properties: {} } },
        { name: "check_proto_sync", description: "Check synchronization between proto files and generated code. Use this tool to verify that generated code (Erlang/TypeScript) matches the current proto file definitions. Detects when proto files have changed but code hasn't been regenerated. Returns sync status and list of files that need regeneration.", inputSchema: { type: "object", properties: {} } },
        { name: "check_proto_sync_fast", description: "Fast proto sync check using SHA256 digests for performance. Use this tool for quick validation that proto files and generated code are in sync. Uses SHA256 hashes to compare proto files with generated code without full parsing. Returns sync status (faster than check_proto_sync but less detailed).", inputSchema: { type: "object", properties: {} } },
        { name: "check_proto_nats_compatibility", description: "Check protobuf message compatibility with NATS JSON encoding requirements. Use this tool to verify that proto messages can be properly serialized/deserialized via NATS JSON protocol. Validates field types, naming conventions, and JSON encoding compatibility. Returns compatibility report with any issues found.", inputSchema: { type: "object", properties: {} } },
        { name: "update_api_registry", description: "Update api-registry.md documentation with current message names from proto files. Use this tool to keep API registry documentation synchronized with actual proto definitions. Scans proto files and updates the registry with message names, services, and enums. Returns update status and list of registered messages.", inputSchema: { type: "object", properties: {} } },
        { name: "update_api_registry_from_proto", description: "Update api-registry.md with real message names extracted directly from proto files. Use this tool to ensure API registry accurately reflects all proto definitions. Parses proto files and extracts message/service/enum names for documentation. Returns update status and registry contents.", inputSchema: { type: "object", properties: {} } },
      ],
    }));

    this.server.setRequestHandler(CallToolRequestSchema, async (request: any) => {
      const { name } = request.params;
      const scripts: Record<string, string> = {
        check_proto: "scripts/check_proto.sh",
        generate_proto_stubs: "scripts/generate_proto_stubs.sh",
        check_proto_sync: "scripts/check_proto_sync.sh",
        check_proto_sync_fast: "scripts/check_proto_sync_fast.sh",
        check_proto_nats_compatibility: "scripts/check_proto_nats_compatibility.sh",
        update_api_registry: "scripts/update_api_registry.sh",
        update_api_registry_from_proto: "scripts/update_api_registry_from_proto.sh",
      };
      if (!scripts[name]) throw new Error(`Unknown tool: ${name}`);
      const result = await this.runner.runScript(scripts[name]);
      return { content: [{ type: "text", text: result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "") }] };
    });
  }

  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("Protobuf MCP Server running");
  }
}

new ProtobufServer().run().catch(console.error);


#!/usr/bin/env node
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
} from "@modelcontextprotocol/sdk/types.js";
import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "./script-runner.js";
import { MCPServerWrapper } from "./mcp-server-wrapper.js";

type ToolResult = CallToolResult;

class StateHistoryMCPServer {
  private server: MCPServerWrapper;
  private runner: ScriptRunner;

  constructor() {
    this.server = new MCPServerWrapper(
      {
        name: "state-history-mcp-server",
        version: "1.0.0",
      },
      {
        capabilities: {
          tools: {},
        },
      }
    );

    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }

  private setupHandlers() {
    // Use wrapper's setRequestHandler (which uses proxy)
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        {
          name: "validate_state",
          description: "Validate .trae/state.json and .trae/history.json files for schema compliance, checksum integrity, and HMAC chain integrity. Use this tool before committing state/history changes to ensure No-Drift policy compliance. Checks JSON schema validation, artifact checksums, and HMAC chain integrity. Returns detailed validation report with any errors or warnings.",
          inputSchema: { type: "object", properties: {} },
        },
        {
          name: "verify_hmac_chain",
          description: "Verify HMAC chain integrity in .trae/history.json file. Use this tool to ensure that the HMAC chain has not been tampered with and all entries are properly signed. This is a critical security check required before committing history changes. Optionally specify HMAC secret or use BEAMLINE_HMAC_SECRET environment variable. Returns verification status with chain integrity details.",
          inputSchema: {
            type: "object",
            properties: {
              secret: { type: "string", description: "HMAC secret (default: from BEAMLINE_HMAC_SECRET env)" },
              verbose: { type: "boolean", description: "Verbose output", default: false },
            },
          },
        },
        {
          name: "recalculate_hmac_chain",
          description: "Recalculate HMAC chain in .trae/history.json with a new secret key. Use this tool when rotating HMAC secrets or fixing HMAC chain integrity issues. Optionally rebuild entire chain from scratch or create backup before recalculation. Requires new secret parameter. Returns recalculation status and backup file path if backup was created.",
          inputSchema: {
            type: "object",
            properties: {
              secret: { type: "string", description: "New secret key" },
              rebuild: { type: "boolean", description: "Rebuild entire chain", default: false },
              backup: { type: "boolean", description: "Create backup before recalculation", default: true },
            },
            required: ["secret"],
          },
        },
        {
          name: "recompute_checksums",
          description: "Recompute SHA256 checksums for artifacts listed in .trae/state.json. Use this tool when artifact files have changed and checksums need to be updated. Optionally specify specific files to recompute, or recompute all artifacts if no files specified. Returns updated checksums and modification timestamps.",
          inputSchema: {
            type: "object",
            properties: {
              files: {
                type: "array",
                items: { type: "string" },
                description: "Specific files to recompute (default: all from artifact_checksums)",
              },
            },
          },
        },
        {
          name: "sign_history",
          description: "Sign the latest history entry in .trae/history.json with HMAC. Use this tool after making state changes to ensure the history entry is properly authenticated. This creates a new HMAC signature for the latest entry using the current HMAC secret. Returns signing status and new HMAC value.",
          inputSchema: { type: "object", properties: {} },
        },
        {
          name: "beamline_store_import",
          description: "Import state.json and/or history.json files into Mnesia database via beamline_store (Erlang DevState). Use this tool to sync local state/history files into Mnesia storage for Erlang/OTP environments. Optionally import only state or only history, or both (default). Returns import status and record counts.",
          inputSchema: {
            type: "object",
            properties: {
              target: { type: "string", enum: ["both", "state", "history"], default: "both" },
              profile: { type: "string", description: "Rebar3 profile (default: default)", default: "default" },
            },
          },
        },
        {
          name: "beamline_store_export",
          description: "Export state.json and/or history.json files from Mnesia database via beamline_store (Erlang DevState). Use this tool to extract state/history data from Mnesia storage for backup or migration purposes. Optionally export only state or only history, or both (default). Returns export status and file paths.",
          inputSchema: {
            type: "object",
            properties: {
              target: { type: "string", enum: ["both", "state", "history"], default: "both" },
              profile: { type: "string", description: "Rebar3 profile (default: default)", default: "default" },
            },
          },
        },
        {
          name: "beamline_store_verify_chain",
          description: "Verify HMAC chain integrity using beamline_store (Erlang DevState). Use this tool to validate HMAC chain stored in Mnesia database for Erlang/OTP environments. This checks that all history entries are properly signed and the chain is intact. Returns verification status with chain integrity details.",
          inputSchema: {
            type: "object",
            properties: {
              profile: { type: "string", description: "Rebar3 profile (default: default)", default: "default" },
            },
          },
        },
      ],
    }));

    this.server.setRequestHandler(CallToolRequestSchema, async (request: any) => {
      const { name, arguments: args } = request.params;

      try {
        let result: ToolResult;

        switch (name) {
          case "validate_state":
            result = await this.runScript("scripts/validate_state.sh");
            break;
          case "verify_hmac_chain": {
            const pyArgs: string[] = [];
            const secret = args?.secret as string | undefined;
            if (secret) pyArgs.push("--secret", secret);
            if (args?.verbose) pyArgs.push("--verbose");
            result = await this.runPythonScript("scripts/verify_hmac_chain.py", pyArgs);
            break;
          }
          case "recalculate_hmac_chain": {
            const secret = args?.secret as string | undefined;
            if (!secret) throw new Error("Secret parameter is required");
            const pyArgs = ["--secret", secret];
            if (args?.rebuild) pyArgs.push("--rebuild");
            if (args?.backup === false) pyArgs.push("--no-backup");
            result = await this.runPythonScript("scripts/recalculate_hmac_chain.py", pyArgs);
            break;
          }
          case "recompute_checksums": {
            const files = (args?.files as string[] | undefined) || [];
            result = await this.runScript(
              "scripts/reports/recompute_checksums.sh",
              {},
              ...files
            );
            break;
          }
          case "sign_history":
            result = await this.runScript("scripts/reports/sign_history.sh");
            break;
          case "beamline_store_import": {
            const target = (args?.target as string | undefined as string | undefined) || "both";
            const profile = (args?.profile as string | undefined as string | undefined) || "default";
            const cmd = target === "both" ? "import" : `import_${target}`;
            result = await this.runScript("scripts/beamline_store_export.sh", { PROFILE: profile }, cmd);
            break;
          }
          case "beamline_store_export": {
            const target = (args?.target as string | undefined as string | undefined) || "both";
            const profile = (args?.profile as string | undefined as string | undefined) || "default";
            const cmd = target === "both" ? "export" : `export_${target}`;
            result = await this.runScript("scripts/beamline_store_export.sh", { PROFILE: profile }, cmd);
            break;
          }
          case "beamline_store_verify_chain": {
            const profile = (args?.profile as string | undefined as string | undefined) || "default";
            result = await this.runScript("scripts/beamline_store_export.sh", { PROFILE: profile }, "verify_chain");
            break;
          }
          default:
            throw new Error(`Unknown tool: ${name}`);
        }

        return result;
      } catch (error: any) {
        return {
          content: [{ type: "text", text: `Error: ${error.message}\n${error.stack || ""}` }],
          isError: true,
        };
      }
    });
  }

  private async runScript(scriptPath: string, env: Record<string, string> = {}, ...args: string[]): Promise<ToolResult> {
    const result = await this.runner.runScript(scriptPath, env, ...args);
    const output = result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "");
    return {
      content: [{ type: "text", text: output }],
      isError: result.exitCode !== 0,
    };
  }

  private async runPythonScript(scriptPath: string, args: string[] = [], env: Record<string, string> = {}): Promise<ToolResult> {
    const result = await this.runner.runPythonScript(scriptPath, args, env);
    const output = result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "");
    return {
      content: [{ type: "text", text: output }],
      isError: result.exitCode !== 0,
    };
  }

  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("State & History MCP Server running on stdio");
  }
}

const server = new StateHistoryMCPServer();
server.run().catch(console.error);


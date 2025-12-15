#!/usr/bin/env node
import { MCPServerWrapper } from "./mcp-server-wrapper.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
  type CallToolResult,
} from "@modelcontextprotocol/sdk/types.js";
import { spawn } from "child_process";
import { promisify } from "util";
import * as fs from "fs/promises";
import * as path from "path";
import { exec } from "child_process";

const execAsync = promisify(exec);

type ToolResult = CallToolResult;

class DevStateMCPServer {
  private server: MCPServerWrapper;
  private projectRoot: string;

  constructor() {
    this.server = new MCPServerWrapper(
      {
        name: "devstate-mcp-server",
        version: "1.0.0",
      },
      {
        capabilities: {
          tools: {},
        },
      }
    );

    // Detect project root (assumes script runs from project root)
    this.projectRoot = process.cwd();
    this.setupHandlers();
  }

  private setupHandlers() {
    // List all available tools
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        {
          name: "devstate_up",
          description: "Start DevState service using Docker Compose. Use this tool when you need to initialize the DevState state management system before working with state.json or history.json files. The service provides HTTP API endpoints for state management, HMAC chain verification, and checkpoint operations. Returns status of Docker containers startup.",
          inputSchema: {
            type: "object",
            properties: {},
          },
        },
        {
          name: "devstate_down",
          description: "Stop DevState service using Docker Compose. Use this tool when you need to shut down the DevState state management system after completing state/history operations. This gracefully stops all DevState containers and releases resources. Returns status of Docker containers shutdown.",
          inputSchema: {
            type: "object",
            properties: {},
          },
        },
        {
          name: "devstate_health",
          description: "Check DevState service health status via HTTP endpoint. Use this tool to verify that DevState service is running and ready to accept requests before performing state/history operations. Returns JSON response with status information (expected: {\"status\":\"ok\"}).",
          inputSchema: {
            type: "object",
            properties: {
              url: {
                type: "string",
                description: "DevState URL (default: http://localhost:3180)",
                default: "http://localhost:3180",
              },
            },
          },
        },
        {
          name: "devstate_verify",
          description: "Verify DevState HMAC chain integrity via HTTP API. Use this tool to validate that the HMAC chain in history.json is intact and has not been tampered with. This is a critical security check required before committing state/history changes. Returns verification status with details about HMAC chain integrity.",
          inputSchema: {
            type: "object",
            properties: {
              url: {
                type: "string",
                description: "DevState URL (default: http://localhost:3180)",
                default: "http://localhost:3180",
              },
            },
          },
        },
        {
          name: "devstate_export",
          description: "Export state.json and history.json files from DevState service. Use this tool after making changes to state/history to sync local files with DevState database. This ensures that local .trae/state.json and .trae/history.json files match the authoritative state stored in DevState. Returns export status and file paths.",
          inputSchema: {
            type: "object",
            properties: {
              url: {
                type: "string",
                description: "DevState URL (default: http://localhost:3180)",
                default: "http://localhost:3180",
              },
            },
          },
        },
        {
          name: "devstate_set_cp",
          description: "Set a checkpoint (CP) marker in DevState. Use this tool to mark completion of a checkpoint phase (e.g., CP1-LC, CP2-LC) in the project lifecycle. Checkpoints are used to track project milestones and enable state rollback. Requires checkpoint name parameter. Returns checkpoint creation status.",
          inputSchema: {
            type: "object",
            properties: {
              cp: {
                type: "string",
                description: "Checkpoint name (e.g., CP1-LC, CP2-LC)",
              },
            },
            required: ["cp"],
          },
        },
        {
          name: "devstate_mnesia_import",
          description: "Import state.json and history.json files into Mnesia database (Erlang DevState implementation). Use this tool when working with the Erlang-based DevState backend to sync local state files into Mnesia storage. This is an alternative to HTTP-based DevState for Erlang/OTP environments. Returns import status.",
          inputSchema: {
            type: "object",
            properties: {},
          },
        },
        {
          name: "devstate_mnesia_export",
          description: "Export state.json and history.json files from Mnesia database to .tmp files (Erlang DevState implementation). Use this tool to extract state/history data from Mnesia storage for backup or migration purposes. This is an alternative to HTTP-based DevState for Erlang/OTP environments. Returns export status and file paths.",
          inputSchema: {
            type: "object",
            properties: {},
          },
        },
      ],
    }));

    // Handle tool calls
    this.server.setRequestHandler(CallToolRequestSchema, async (request: any) => {
      const { name, arguments: args } = request.params;
      if (!args) {
        throw new Error("Arguments are required");
      }

      try {
        let result: ToolResult;

        switch (name) {
          case "devstate_up":
            result = await this.runMakeCommand("devstate-up");
            break;
          case "devstate_down":
            result = await this.runMakeCommand("devstate-down");
            break;
          case "devstate_health": {
            const url = (args?.url as string | undefined) || "http://localhost:3180";
            result = await this.runCurlHealth(url);
            break;
          }
          case "devstate_verify": {
            const url = (args?.url as string | undefined) || "http://localhost:3180";
            result = await this.runScript("devstate/scripts/devstate_verify.sh", {
              DEVSTATE_URL: url,
            });
            break;
          }
          case "devstate_export": {
            const url = (args?.url as string | undefined) || "http://localhost:3180";
            result = await this.runScript("devstate/scripts/devstate_export.sh", {
              DEVSTATE_URL: url,
            });
            break;
          }
          case "devstate_set_cp": {
            const cp = args?.cp as string | undefined;
            if (!cp) {
              throw new Error("CP parameter is required");
            }
            result = await this.runScript("devstate/scripts/devstate_set_cp.sh", {
              CP: cp,
            });
            break;
          }
          case "devstate_mnesia_import":
            result = await this.runScript("devstate/scripts/devstate.sh", {}, "import");
            break;
          case "devstate_mnesia_export":
            result = await this.runScript("devstate/scripts/devstate.sh", {}, "export");
            break;
          default:
            throw new Error(`Unknown tool: ${name}`);
        }

        return result;
      } catch (error: any) {
        return {
          content: [
            {
              type: "text",
              text: `Error executing ${name}: ${error.message}\n${error.stack || ""}`,
            },
          ],
          isError: true,
        };
      }
    });
  }

  private async runScript(
    scriptPath: string,
    env: Record<string, string> = {},
    ...args: string[]
  ): Promise<ToolResult> {
    const fullPath = path.join(this.projectRoot, scriptPath);

    // Check if script exists
    try {
      await fs.access(fullPath);
    } catch {
      throw new Error(`Script not found: ${scriptPath}`);
    }

    const { stdout, stderr } = await execAsync(
      `bash "${fullPath}" ${args.map((a) => `"${a}"`).join(" ")}`,
      {
        cwd: this.projectRoot,
        env: { ...process.env, ...env },
        maxBuffer: 10 * 1024 * 1024, // 10MB
      }
    );

    const output = stdout + (stderr ? `\n[stderr]\n${stderr}` : "");
    return {
      content: [{ type: "text", text: output }],
    };
  }

  private async runMakeCommand(target: string): Promise<ToolResult> {
    const { stdout, stderr } = await execAsync(`make ${target}`, {
      cwd: this.projectRoot,
      env: process.env,
    });

    const output = stdout + (stderr ? `\n[stderr]\n${stderr}` : "");
    return {
      content: [{ type: "text", text: output }],
    };
  }

  private async runCurlHealth(url: string): Promise<ToolResult> {
    try {
      const { stdout, stderr } = await execAsync(`curl -fsS ${url}/health`, {
        cwd: this.projectRoot,
        env: process.env,
      });

      const output = stdout + (stderr ? `\n[stderr]\n${stderr}` : "");
      return {
        content: [{ type: "text", text: output }],
      };
    } catch (error: any) {
      // Try fallback URL
      const fallbackUrl = "http://localhost:3080";
      try {
        const { stdout, stderr } = await execAsync(`curl -fsS ${fallbackUrl}/health`, {
          cwd: this.projectRoot,
          env: process.env,
        });
        const output = stdout + (stderr ? `\n[stderr]\n${stderr}` : "");
        return {
          content: [{ type: "text", text: output }],
        };
      } catch {
        throw new Error(`Health check failed for ${url} and ${fallbackUrl}`);
      }
    }
  }

  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("DevState MCP Server running on stdio");
  }
}

// Start server
const server = new DevStateMCPServer();
server.run().catch(console.error);


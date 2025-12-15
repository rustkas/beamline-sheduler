#!/usr/bin/env node
import { MCPServerWrapper } from "./mcp-server-wrapper.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
} from "@modelcontextprotocol/sdk/types.js";
import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "./script-runner.js";

type ToolResult = CallToolResult;

class WindsurfHooksMCPServer {
  private server: MCPServerWrapper;
  private runner: ScriptRunner;

  constructor() {
    this.server = new MCPServerWrapper(
      {
        name: "windsurf-hooks-mcp-server",
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
    // List all available tools
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        {
          name: "windsurf_recommend_model",
          description: "Recommend appropriate AI model based on changed files analysis (Windsurf hook). Use this tool to automatically select the best AI model for code changes by analyzing file types, change scope, and complexity. This hook is called by Windsurf IDE to optimize model selection. Requires JSON payload in Windsurf hook format. Returns recommended model name and reasoning.",
          inputSchema: {
            type: "object",
            properties: {
              stdin_data: {
                type: "string",
                description: "JSON payload from stdin (Windsurf hook format)",
                default: "{}",
              },
            },
          },
        },
        {
          name: "windsurf_block_read",
          description: "Block read access to files outside allowlist directories (Windsurf policy hook). Use this tool to enforce security policies by preventing AI from reading files outside specified directories. This hook is called by Windsurf IDE to validate file access permissions. Requires JSON payload in Windsurf hook format. Returns access decision (allow/block) with reason.",
          inputSchema: {
            type: "object",
            properties: {
              stdin_data: {
                type: "string",
                description: "JSON payload from stdin (Windsurf hook format)",
                default: "{}",
              },
            },
          },
        },
        {
          name: "list_windsurf_hooks",
          description: "List all Windsurf hooks configured in .windsurf/hooks.json file. Use this tool to discover available Windsurf IDE hooks and their configurations. Shows hook names, scripts, triggers, and execution conditions. Returns list of configured hooks with their details.",
          inputSchema: {
            type: "object",
            properties: {},
          },
        },
        {
          name: "run_check_cp1_contracts",
          description: "Run CP1 contracts check script as used in Windsurf hooks. Use this tool to execute CP1 contract validation from Windsurf IDE hooks or manually. Validates that all components meet CP1 contract requirements. Returns CP1 contract validation report with any violations found.",
          inputSchema: {
            type: "object",
            properties: {},
          },
        },
        {
          name: "run_check_cp2_behavior",
          description: "Run CP2 behavior check script as used in Windsurf hooks. Use this tool to execute CP2 behavior validation from Windsurf IDE hooks or manually. Validates that CP2 features behave correctly according to specifications. Returns CP2 behavior validation report with any discrepancies found.",
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

      try {
        let result: ToolResult;

        switch (name) {
          case "windsurf_recommend_model": {
            const stdinData = (args?.stdin_data as string | undefined) || "{}";
            result = await this.runPythonScript(
              "scripts/windsurf_hooks/recommend_model.py",
              [],
              { stdin_data: stdinData }
            );
            break;
          }
          case "windsurf_block_read": {
            const stdinData = (args?.stdin_data as string | undefined) || "{}";
            result = await this.runPythonScript(
              "scripts/windsurf_hooks/block_read_outside_allow.py",
              [],
              { stdin_data: stdinData }
            );
            break;
          }
          case "list_windsurf_hooks": {
            result = await this.runScript("scripts/windsurf_hooks/list_hooks.sh");
            break;
          }
          case "run_check_cp1_contracts": {
            result = await this.runScript("scripts/check_cp1_contracts.sh");
            break;
          }
          case "run_check_cp2_behavior": {
            result = await this.runScript("scripts/check_cp2_behavior.sh");
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

  private async runScript(
    scriptPath: string,
    env: Record<string, string> = {},
    ...args: string[]
  ): Promise<ToolResult> {
    const result = await this.runner.runScript(scriptPath, env, ...args);
    const output = result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "");
    return {
      content: [{ type: "text", text: output }],
      isError: result.exitCode !== 0,
    };
  }

  private async runPythonScript(
    scriptPath: string,
    args: string[] = [],
    env: Record<string, string> = {}
  ): Promise<ToolResult> {
    // For Python scripts that need stdin, we need special handling
    const stdinData = env.stdin_data || "{}";
    delete env.stdin_data; // Remove from env, will pass via stdin
    
    const result = await this.runner.runPythonScript(scriptPath, args, env, stdinData);
    const output = result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "");
    return {
      content: [{ type: "text", text: output }],
      isError: result.exitCode !== 0,
    };
  }

  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("Windsurf Hooks MCP Server running on stdio");
  }
}

const server = new WindsurfHooksMCPServer();
server.run().catch(console.error);


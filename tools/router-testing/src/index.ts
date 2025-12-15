#!/usr/bin/env node
import { MCPServerWrapper } from "./mcp-server-wrapper.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { CallToolRequestSchema, ListToolsRequestSchema } from "@modelcontextprotocol/sdk/types.js";
import type { CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { ScriptRunner } from "./script-runner.js";

class RouterTestingServer {
  private server: MCPServerWrapper;
  private runner: ScriptRunner;
  constructor() {
    this.server = new MCPServerWrapper({ name: "router-testing-mcp", version: "1.0.0" }, { capabilities: { tools: {} } });
    this.runner = new ScriptRunner(process.cwd());
    this.setupHandlers();
  }
  private setupHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        {
          name: "router_test_fast",
          description: "Run fast Router test suites for quick validation during development. Use this tool to execute quick unit and integration tests that complete in seconds. These tests cover basic functionality and common code paths. Optionally enable verbose output for detailed test results. Returns test execution report with pass/fail status and execution time.",
          inputSchema: { type: "object", properties: { verbose: { type: "boolean", default: false } } },
        },
        {
          name: "router_test_slow",
          description: "Run slow Router test suites including comprehensive integration and end-to-end tests. Use this tool for thorough validation before releases or when investigating complex issues. These tests may take minutes to complete and cover edge cases, performance scenarios, and full system integration. Optionally enable verbose output. Returns detailed test execution report.",
          inputSchema: { type: "object", properties: { verbose: { type: "boolean", default: false } } },
        },
        {
          name: "router_test_cp1_smoke",
          description: "Run CP1 (Checkpoint 1) baseline smoke tests to verify Router meets CP1 contract requirements. Use this tool to validate that Router implementation satisfies CP1 contracts and baseline functionality. These tests ensure Router is ready for CP1 checkpoint completion. Optionally enable verbose output. Returns CP1 smoke test report with contract compliance status.",
          inputSchema: { type: "object", properties: { verbose: { type: "boolean", default: false } } },
        },
        {
          name: "router_list_tests_by_tag",
          description: "List Router test suites filtered by tag expression. Use this tool to discover available tests matching specific criteria (e.g., by feature, component, or test type). Tag expressions allow filtering tests before execution. Requires tag_expr parameter. Returns list of matching test suites with their tags and descriptions.",
          inputSchema: { type: "object", properties: { tag_expr: { type: "string" } }, required: ["tag_expr"] },
        },
        {
          name: "router_test_profile",
          description: "Profile Router test suites to analyze performance and identify bottlenecks. Use this tool to measure test execution time, memory usage, and identify slow tests. Optionally specify test mode (fast, cp1-smoke, slow, jetstream, all) or specific test suites. Returns profiling report with performance metrics and recommendations.",
          inputSchema: {
            type: "object",
            properties: {
              mode: { type: "string", enum: ["fast", "cp1-smoke", "slow", "jetstream", "all"], default: "fast" },
              suites: { type: "array", items: { type: "string" } },
            },
          },
        },
        {
          name: "router_contract_snapshot",
          description: "Perform static check that documented contracts match Router implementation. Use this tool to verify that API contracts documented in specifications match actual Router code implementation. Detects contract drift and ensures documentation accuracy. Optionally enable verbose output or specify output file. Returns contract validation report with any mismatches found.",
          inputSchema: { type: "object", properties: { verbose: { type: "boolean", default: false }, output: { type: "string" } } },
        },
        {
          name: "router_regression_snapshot",
          description: "Generate regression snapshot report comparing current Router behavior with baseline. Use this tool to detect behavioral changes and regressions in Router functionality. Compares current test results with baseline snapshot or specified comparison point. Optionally create new baseline or compare with specific snapshot. Returns regression report with detected changes.",
          inputSchema: {
            type: "object",
            properties: { baseline: { type: "boolean", default: true }, compare: { type: "string" } },
          },
        },
        {
          name: "gateway_router_contract_smoke",
          description: "Run end-to-end smoke test for Gateway ↔ Router contract to verify integration. Use this tool to validate that Gateway and Router communicate correctly according to contract specifications. Tests message passing, error handling, and contract compliance. Optionally test router-only, gateway-only, or full integration (default). Returns contract smoke test report with integration status.",
          inputSchema: {
            type: "object",
            properties: { mode: { type: "string", enum: ["router-only", "gateway-only", "full"], default: "full" } },
          },
        },
        {
          name: "devstate_router_fallback_smoke",
          description: "Verify Router fallback behavior when DevState is unavailable or returns invalid data. Use this tool to test Router resilience and fallback mechanisms for various failure scenarios (missing state, invalid JSON, no_drift flag false). Optionally test specific scenario or all scenarios (default: missing_state). Returns fallback behavior validation report.",
          inputSchema: {
            type: "object",
            properties: {
              scenario: { type: "string", enum: ["missing_state", "invalid_json", "no_drift_false", "all"], default: "missing_state" },
            },
          },
        },
        {
          name: "router_check_test_location",
          description: "Check if Router tests are being run from correct directory. Use this tool to verify test execution context and ensure tests have access to required files and configurations. Validates working directory, file paths, and test environment setup. Returns location validation report with any path or context issues.",
          inputSchema: { type: "object", properties: {} },
        },
      ],
    }));
    this.server.setRequestHandler(CallToolRequestSchema, async (request: any) => {
      const { name, arguments: args } = request.params;
      let result;
      if (name === "router_test_fast") {
        result = await this.runner.runScript("apps/otp/router/scripts/test_fast.sh", {}, ...(args?.verbose as boolean | undefined ? ["--verbose"] : []));
      } else if (name === "router_test_slow") {
        result = await this.runner.runScript("apps/otp/router/scripts/test_slow.sh", {}, ...(args?.verbose as boolean | undefined ? ["--verbose"] : []));
      } else if (name === "router_test_cp1_smoke") {
        result = await this.runner.runScript("apps/otp/router/scripts/test_cp1_smoke.sh", {}, ...(args?.verbose as boolean | undefined ? ["--verbose"] : []));
      } else if (name === "router_list_tests_by_tag") {
        const tagExpr = args?.tag_expr as string | undefined;
        if (!tagExpr) throw new Error("tag_expr parameter is required");
        result = await this.runner.runScript("apps/otp/router/scripts/list_tests_by_tag.sh", {}, tagExpr);
      } else if (name === "router_test_profile") {
        const mode = args?.mode as string | undefined || "fast";
        const scriptArgs = [`--${mode}`];
        const suites = (args?.suites as string[] | undefined);
        if (suites) scriptArgs.push("--suites", suites.join(","));
        result = await this.runner.runScript("scripts/router_test_profile.sh", {}, ...scriptArgs);
      } else if (name === "router_contract_snapshot") {
        const scriptArgs: string[] = [];
        if (args?.verbose as boolean | undefined) scriptArgs.push("--verbose");
        const output = args?.output as string | undefined;
        if (output) scriptArgs.push("--output", output);
        result = await this.runner.runScript("scripts/router_contract_snapshot.sh", {}, ...scriptArgs);
      } else if (name === "router_regression_snapshot") {
        const scriptArgs: string[] = [];
        if (args?.baseline as boolean | undefined) scriptArgs.push("--baseline");
        const compare = args?.compare as string | undefined;
        if (compare) scriptArgs.push("--compare", compare);
        result = await this.runner.runScript("scripts/router_regression_snapshot.sh", {}, ...scriptArgs);
      } else if (name === "gateway_router_contract_smoke") {
        const mode = args?.mode as string | undefined || "full";
        result = await this.runner.runScript("scripts/gateway_router_contract_smoke.sh", {}, `--${mode}`);
      } else if (name === "devstate_router_fallback_smoke") {
        const scenario = args?.scenario as string | undefined || "missing_state";
        result = await this.runner.runScript("scripts/devstate_router_fallback_smoke.sh", {}, "--scenario", scenario);
      } else if (name === "router_check_test_location") {
        result = await this.runner.runScript("apps/otp/router/scripts/check_test_run_location.sh");
      } else {
        throw new Error(`Unknown tool: ${name}`);
      }
      return { content: [{ type: "text", text: result.stdout + (result.stderr ? `\n[stderr]\n${result.stderr}` : "") }] };
    });
  }
  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("Router Testing MCP Server running");
  }
}
new RouterTestingServer().run().catch(console.error);


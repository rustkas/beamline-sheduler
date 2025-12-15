/**
 * MCP Server Wrapper
 * 
 * NOTE: This wrapper does NOT fix Trae IDE bug.
 * Trae IDE performs static code analysis and sees SDK Server class directly.
 * 
 * This wrapper is kept for code organization, but Trae will still count SDK methods.
 * 
 * The real solution: Report bug to Trae IDE developers or use Cursor IDE.
 */

import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import type { ServerOptions } from "@modelcontextprotocol/sdk/server/index.js";
import type { Implementation } from "@modelcontextprotocol/sdk/types.js";

/**
 * Wrapper around MCP Server.
 * 
 * ⚠️ LIMITATION: Trae IDE performs static analysis on imports.
 * It sees the Server class from @modelcontextprotocol/sdk directly,
 * so this wrapper cannot hide SDK methods from Trae's static analysis.
 * 
 * This wrapper is kept for:
 * - Code organization
 * - Future compatibility if Trae fixes the bug
 * - Consistency across all servers
 */
export class MCPServerWrapper {
  private server: Server;

  constructor(serverInfo: Implementation, options: ServerOptions) {
    this.server = new Server(serverInfo, options);
  }

  // Expose essential MCP protocol methods
  setRequestHandler(schema: any, handler: any): void {
    return this.server.setRequestHandler(schema, handler);
  }

  setNotificationHandler(schema: any, handler: any): void {
    return this.server.setNotificationHandler(schema, handler);
  }

  async connect(transport: any): Promise<void> {
    return this.server.connect(transport);
  }

  // Internal server reference (for advanced use cases)
  getServer(): Server {
    return this.server;
  }
}

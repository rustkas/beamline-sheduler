import { exec } from "child_process";
import { promisify } from "util";
import * as fs from "fs/promises";
import * as path from "path";

const execAsync = promisify(exec);

export interface ScriptResult {
  stdout: string;
  stderr: string;
  exitCode: number;
}

export class ScriptRunner {
  constructor(private projectRoot: string) {}

  async runScript(
    scriptPath: string,
    env: Record<string, string> = {},
    ...args: string[]
  ): Promise<ScriptResult> {
    const fullPath = path.join(this.projectRoot, scriptPath);
    
    // Check if script exists
    try {
      await fs.access(fullPath);
    } catch {
      throw new Error(`Script not found: ${scriptPath}`);
    }

    const escapedArgs = args.map((a) => `"${a.replace(/"/g, '\\"')}"`).join(" ");
    const command = `bash "${fullPath}" ${escapedArgs}`;

    try {
      const { stdout, stderr } = await execAsync(command, {
        cwd: this.projectRoot,
        env: { ...process.env, ...env },
        maxBuffer: 10 * 1024 * 1024, // 10MB
      });
      return { stdout, stderr, exitCode: 0 };
    } catch (error: any) {
      return {
        stdout: error.stdout || "",
        stderr: error.stderr || error.message || "",
        exitCode: error.code || 1,
      };
    }
  }

  async runPythonScript(
    scriptPath: string,
    args: string[] = [],
    env: Record<string, string> = {}
  ): Promise<ScriptResult> {
    const fullPath = path.join(this.projectRoot, scriptPath);
    
    try {
      await fs.access(fullPath);
    } catch {
      throw new Error(`Script not found: ${scriptPath}`);
    }

    const command = `python3 "${fullPath}" ${args.join(" ")}`;

    try {
      const { stdout, stderr } = await execAsync(command, {
        cwd: this.projectRoot,
        env: { ...process.env, ...env },
        maxBuffer: 10 * 1024 * 1024,
      });
      return { stdout, stderr, exitCode: 0 };
    } catch (error: any) {
      return {
        stdout: error.stdout || "",
        stderr: error.stderr || error.message || "",
        exitCode: error.code || 1,
      };
    }
  }

  async runNodeScript(
    scriptPath: string,
    args: string[] = [],
    env: Record<string, string> = {}
  ): Promise<ScriptResult> {
    const fullPath = path.join(this.projectRoot, scriptPath);
    
    try {
      await fs.access(fullPath);
    } catch {
      throw new Error(`Script not found: ${scriptPath}`);
    }

    const command = `node "${fullPath}" ${args.join(" ")}`;

    try {
      const { stdout, stderr } = await execAsync(command, {
        cwd: this.projectRoot,
        env: { ...process.env, ...env },
        maxBuffer: 10 * 1024 * 1024,
      });
      return { stdout, stderr, exitCode: 0 };
    } catch (error: any) {
      return {
        stdout: error.stdout || "",
        stderr: error.stderr || error.message || "",
        exitCode: error.code || 1,
      };
    }
  }

  async runMakeCommand(target: string, env: Record<string, string> = {}): Promise<ScriptResult> {
    try {
      const { stdout, stderr } = await execAsync(`make ${target}`, {
        cwd: this.projectRoot,
        env: { ...process.env, ...env },
      });
      return { stdout, stderr, exitCode: 0 };
    } catch (error: any) {
      return {
        stdout: error.stdout || "",
        stderr: error.stderr || error.message || "",
        exitCode: error.code || 1,
      };
    }
  }

  async runCurl(url: string, options: string[] = []): Promise<ScriptResult> {
    const command = `curl ${options.join(" ")} "${url}"`;
    try {
      const { stdout, stderr } = await execAsync(command, {
        cwd: this.projectRoot,
        env: process.env,
      });
      return { stdout, stderr, exitCode: 0 };
    } catch (error: any) {
      return {
        stdout: error.stdout || "",
        stderr: error.stderr || error.message || "",
        exitCode: error.code || 1,
      };
    }
  }
}


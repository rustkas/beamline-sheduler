import { exec, spawn } from "child_process";
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

  async runPythonScript(
    scriptPath: string,
    args: string[] = [],
    env: Record<string, string> = {},
    stdinData: string = ""
  ): Promise<ScriptResult> {
    const fullPath = path.join(this.projectRoot, scriptPath);
    
    try {
      await fs.access(fullPath);
    } catch {
      throw new Error(`Script not found: ${scriptPath}`);
    }

    // For Python scripts that need stdin, use spawn with stdin
    return new Promise<ScriptResult>((resolve) => {
      const pythonArgs = [fullPath, ...args];
      const childProcess = spawn("python3", pythonArgs, {
        cwd: this.projectRoot,
        env: { ...process.env, ...env },
      });

      let stdout = "";
      let stderr = "";

      childProcess.stdout.on("data", (data: Buffer) => {
        stdout += data.toString();
      });

      childProcess.stderr.on("data", (data: Buffer) => {
        stderr += data.toString();
      });

      childProcess.on("close", (code: number | null) => {
        resolve({
          stdout,
          stderr,
          exitCode: code || 0,
        });
      });

      // Write stdin data if provided
      if (stdinData && childProcess.stdin) {
        childProcess.stdin.write(stdinData);
        childProcess.stdin.end();
      }
    });
  }
}


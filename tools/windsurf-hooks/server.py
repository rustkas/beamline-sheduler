#!/usr/bin/env python3
"""
Windsurf Hooks MCP Server
Wraps Windsurf IDE integration hooks (model recommendation, access control)
"""
import json
import sys
import subprocess
from typing import Any, Dict

def run_script(script_path: str, stdin_data: str = "") -> Dict[str, Any]:
    """Run a Python script with stdin input"""
    try:
        process = subprocess.Popen(
            ["python3", script_path],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            cwd="."
        )
        stdout, stderr = process.communicate(input=stdin_data)
        return {
            "stdout": stdout,
            "stderr": stderr,
            "exit_code": process.returncode
        }
    except Exception as e:
        return {
            "stdout": "",
            "stderr": str(e),
            "exit_code": 1
        }

def handle_mcp_request(request: Dict[str, Any]) -> Dict[str, Any]:
    """Handle MCP request"""
    method = request.get("method")
    params = request.get("params", {})
    
    if method == "tools/list":
        return {
            "jsonrpc": "2.0",
            "id": request.get("id"),
            "result": {
                "tools": [
                    {
                        "name": "windsurf_recommend_model",
                        "description": "Recommend AI model based on changed files",
                        "inputSchema": {
                            "type": "object",
                            "properties": {
                                "stdin_data": {
                                    "type": "string",
                                    "description": "JSON payload from stdin (Windsurf hook format)"
                                }
                            }
                        }
                    },
                    {
                        "name": "windsurf_block_read",
                        "description": "Block read access outside allowlist directories (policy hook)",
                        "inputSchema": {
                            "type": "object",
                            "properties": {
                                "stdin_data": {
                                    "type": "string",
                                    "description": "JSON payload from stdin (Windsurf hook format)"
                                }
                            }
                        }
                    }
                ]
            }
        }
    
    elif method == "tools/call":
        tool_name = params.get("name")
        args = params.get("arguments", {})
        stdin_data = args.get("stdin_data", "{}")
        
        if tool_name == "windsurf_recommend_model":
            result = run_script("scripts/windsurf_hooks/recommend_model.py", stdin_data)
        elif tool_name == "windsurf_block_read":
            result = run_script("scripts/windsurf_hooks/block_read_outside_allow.py", stdin_data)
        else:
            return {
                "jsonrpc": "2.0",
                "id": request.get("id"),
                "error": {"code": -32601, "message": f"Unknown tool: {tool_name}"}
            }
        
        output = result["stdout"] + (f"\n[stderr]\n{result['stderr']}" if result["stderr"] else "")
        
        return {
            "jsonrpc": "2.0",
            "id": request.get("id"),
            "result": {
                "content": [{"type": "text", "text": output}],
                "isError": result["exit_code"] != 0
            }
        }
    
    else:
        return {
            "jsonrpc": "2.0",
            "id": request.get("id"),
            "error": {"code": -32601, "message": f"Unknown method: {method}"}
        }

def main():
    """Main MCP server loop"""
    for line in sys.stdin:
        try:
            request = json.loads(line.strip())
            response = handle_mcp_request(request)
            print(json.dumps(response))
            sys.stdout.flush()
        except json.JSONDecodeError:
            error_response = {
                "jsonrpc": "2.0",
                "id": None,
                "error": {"code": -32700, "message": "Parse error"}
            }
            print(json.dumps(error_response))
            sys.stdout.flush()
        except Exception as e:
            error_response = {
                "jsonrpc": "2.0",
                "id": None,
                "error": {"code": -32603, "message": f"Internal error: {str(e)}"}
            }
            print(json.dumps(error_response))
            sys.stdout.flush()

if __name__ == "__main__":
    main()


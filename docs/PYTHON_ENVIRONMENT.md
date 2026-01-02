# Python Environment Setup

## Overview

This project uses **Python 3.10+** for devstate-related services and utility scripts.

## Version Requirements

- **Python**: 3.10 or higher (specified in `.python-version` files)
- **Package Manager**: pip3 (or pip within activated venv)

## Python Location

- **Root**: `.python-version` specifies `3.10` for pyenv/asdf tools
- **DevState Viewer**: `devstate-tools/devstate-viewer/.python-version` specifies `3.10` for devstate-viewer specific tools

## DevState Viewer Setup

The DevState Viewer FastAPI service is located in `devstate-tools/devstate-viewer/`:

```bash
cd devstate-tools/devstate-viewer

# Create virtual environment
python3 -m venv .venv

# Activate virtual environment
# Linux/macOS:
source .venv/bin/activate
# Windows:
.venv\Scripts\activate

# Install dependencies
pip3 install -r requirements.txt

# Install dev dependencies (for testing, linting, type checking)
pip3 install -r requirements-dev.txt
```

### Configuration Files

- `devstate-tools/devstate-viewer/pyproject.toml`: Project metadata and tool configuration
  - Specifies `requires-python = ">=3.10"`
  - Configures black, ruff, mypy, pytest
- `devstate-tools/devstate-viewer/requirements.txt`: Production dependencies
- `devstate-tools/devstate-viewer/requirements-dev.txt`: Development dependencies

### Testing and Linting

```bash
cd devstate-tools/devstate-viewer

# Run tests
pytest

# Run tests with coverage
pytest --cov=app --cov-report=html

# Format code
black app/ tests/

# Lint code
ruff check app/ tests/

# Type checking
mypy app/
```

## Utility Scripts

All Python utility scripts are located in `scripts/` and use `#!/usr/bin/env python3` shebang:

- `scripts/check_secret_compatibility.py`: Verify HMAC secret compatibility
- `scripts/recalculate_hmac_chain.py`: Recalculate HMAC chain with new secret
- `scripts/verify_hmac_chain.py`: Verify HMAC chain integrity
- `scripts/windsurf_hooks/block_read_outside_allow.py`: Pre-read hook for access control
- `scripts/windsurf_hooks/recommend_model.py`: Model recommendation hook

### Running Utility Scripts

All scripts are executable and use the system's `python3`:

```bash
# Direct execution (requires execute permission)
./scripts/verify_hmac_chain.py

# Or explicit python3 invocation
python3 scripts/verify_hmac_chain.py
```

## Shell Scripts Integration

All shell scripts in `scripts/*.sh` consistently use `python3` for Python invocations:

- `validate_state.sh`: Uses `python3 -m json.tool` and inline Python scripts
- `check_version_gates.sh`: Calls `python3 scripts/verify_hmac_chain.py`
- `dry_run_ci.sh`: Invokes Python validators with `python3`
- `observability/*.sh`: Uses `python3` with fallback checks

## Consistency Rules

1. **Always use `python3`**, never `python` (for compatibility with systems where `python` → Python 2.x)
2. **Always use `pip3`** for package installation outside venv
3. **Shebang line**: `#!/usr/bin/env python3` in all Python scripts
4. **No indentation before shebang**: Shebang must be at column 0
5. **Version pinning**: Use `.python-version` files for pyenv/asdf compatibility

## IDE/Editor Configuration

The project uses Windsurf IDE which respects `.python-version` files. Each workspace folder can specify its Python interpreter:

- **aigroup** (root): Uses `devstate-tools/devstate-viewer/.venv/bin/python` if venv exists, else system `python3`
- **Other workspaces**: Use system `/bin/python` (should be Python 3.x)

### VSCode/PyCharm Users

If you use VSCode or PyCharm:

1. Point the Python interpreter to `devstate-tools/devstate-viewer/.venv/bin/python` for devstate-viewer work
2. Ensure your editor respects `.python-version` files for version selection
3. Enable formatters: black (line-length 100)
4. Enable linters: ruff, mypy

## Validation

To verify Python setup consistency:

```bash
# Check Python version
python3 --version  # Should be 3.10+

# Check all script shebangs
grep -r "^#!/usr/bin/env python3" scripts/

# Run devstate-viewer checks
cd devstate-tools/devstate-viewer
python3 -m pytest
python3 -m black --check app/ tests/
python3 -m ruff check app/ tests/
python3 -m mypy app/
```

## Troubleshooting

### "python: command not found"

- Ensure you use `python3` explicitly
- On some systems, install via: `sudo apt install python3 python3-pip python3-venv`

### Virtual environment issues

```bash
# Remove old venv
rm -rf devstate-tools/devstate-viewer/.venv

# Recreate with correct Python version
cd devstate-tools/devstate-viewer
python3 -m venv .venv
source .venv/bin/activate
pip3 install --upgrade pip
pip3 install -r requirements.txt -r requirements-dev.txt
```

### pyenv/asdf users

If using pyenv or asdf for Python version management:

```bash
# Install Python 3.10+
pyenv install 3.10.13
# or
asdf install python 3.10.13

# Set local version (reads .python-version)
pyenv local 3.10
# or
asdf local python 3.10.13
```

## Dependencies

### DevState Viewer (FastAPI)

Core production dependencies (see `devstate-tools/devstate-viewer/requirements.txt`):
- `fastapi>=0.104.0`
- `uvicorn[standard]>=0.24.0`
- `pydantic>=2.5.0`
- `pydantic-settings>=2.1.0`
- `jsonschema>=4.20.0`

Dev dependencies (see `devstate-tools/devstate-viewer/requirements-dev.txt`):
- `pytest>=7.4.0`
- `pytest-asyncio>=0.21.0`
- `pytest-cov>=4.1.0`
- `httpx>=0.25.0`
- `black>=23.11.0`
- `ruff>=0.1.6`
- `mypy>=1.7.0`

### Scripts

Utility scripts have minimal external dependencies (standard library only):
- `json`, `hmac`, `hashlib`, `subprocess`, `pathlib`

For observability validators:
- `jsonschema` (optional, for log schema validation)

## CI/CD

CI environments should:

1. Install Python 3.10+ explicitly
2. Create venv for DevState Viewer tests
3. Run all checks via `scripts/run_checks.sh`
4. Validate Python consistency with this document

Example CI steps:

```yaml
- name: Setup Python
  uses: actions/setup-python@v4
  with:
    python-version: '3.10'

- name: Install DevState Viewer dependencies
  run: |
    cd devstate-tools/devstate-viewer
    python3 -m venv .venv
    source .venv/bin/activate
    pip3 install -r requirements.txt -r requirements-dev.txt

- name: Run DevState Viewer tests
  run: |
    cd devstate-tools/devstate-viewer
    source .venv/bin/activate
    pytest --cov=app --cov-report=xml

- name: Run utility script checks
  run: |
    python3 scripts/verify_hmac_chain.py
    bash scripts/validate_state.sh
```

## References

- DevState Viewer configuration: `devstate-tools/devstate-viewer/pyproject.toml`
- Python version files: `.python-version`, `devstate-tools/devstate-viewer/.python-version`
- DevState Viewer README: `devstate-tools/devstate-viewer/README.md`
- Scripts directory: `scripts/`

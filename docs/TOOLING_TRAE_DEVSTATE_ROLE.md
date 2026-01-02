---
# TRAE IDE and DevState: Development Tooling for BeamLine Constructor

## Role in the Project

TRAE IDE and DevState are **external development tooling** for the development environment and **do not enter into the product logic** of BeamLine Constructor.

## What This Is

### TRAE IDE
- **Development environment** with built-in AI assistant
- Provides **development orchestration** through agents and checkpoints
- Manages the **development lifecycle**, not the product

### DevState
- **Development state management utility** in the TRAE environment
- Works with `.trae/state.json` and `.trae/history.json` files
- Provides **No-Drift control** and **HMAC-audit** at the development level
- **Service for IDE**, not for the BeamLine product

## Responsibility Boundaries

| What It Does | Who Is Responsible |
|--------------|-------------------|
| **Development state management** | TRAE IDE + DevState |
| **AI process orchestration** | BeamLine Constructor (Erlang/OTP + CAF/C++) |
| **Development history storage** | DevState (external tool) |
| **AI agent execution** | BeamLine Worker (CAF/C++) |
| **Request routing** | BeamLine Router (Erlang/OTP) |
| **API Gateway** | BeamLine Gateway (NestJS) |

## Key Difference

- **BeamLine Constructor** = product for AI orchestration
- **TRAE + DevState** = tooling for developing this product

## Practical Application

1. **Developer uses TRAE** to write BeamLine code
2. **DevState tracks** state and change history
3. **BeamLine Constructor** works **independently** of DevState
4. **Production system** does not depend on TRAE or DevState

## Important to Remember

- DevState can be **replaced** with another state management tool
- TRAE can be **replaced** with another IDE
- **BeamLine Constructor will continue to work** without them
- **Product logic** resides **only in BeamLine components**
#!/bin/bash
# Генератор базовых package.json и tsconfig.json для всех MCP серверов

SERVERS=(
  "schema-validation"
  "protobuf"
  "security"
  "compliance"
  "observability"
  "infrastructure"
  "documentation"
  "release-management"
  "ci-validation"
  "cp-validation"
  "router-testing"
  "project-status"
)

for server in "${SERVERS[@]}"; do
  cat > "tools/$server/package.json" << PKGEOF
{
  "name": "@beamline/mcp-${server//-/_}",
  "version": "1.0.0",
  "description": "MCP server for ${server//-/ }",
  "type": "module",
  "main": "dist/index.js",
  "scripts": {
    "build": "tsc",
    "start": "node dist/index.js"
  },
  "dependencies": {
    "@modelcontextprotocol/sdk": "^0.5.0"
  },
  "devDependencies": {
    "@types/node": "^20.0.0",
    "typescript": "^5.0.0"
  }
}
PKGEOF

  cat > "tools/$server/tsconfig.json" << TSCEOF
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "ES2022",
    "lib": ["ES2022"],
    "moduleResolution": "node",
    "outDir": "./dist",
    "rootDir": "./src",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "resolveJsonModule": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist"]
}
TSCEOF
done

echo "Generated package.json and tsconfig.json for ${#SERVERS[@]} servers"

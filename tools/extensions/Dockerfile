FROM node:20-alpine

WORKDIR /app

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm install

# Copy source files
COPY src/ ./src/

# Default command (can be overridden)
CMD ["node", "src/normalize_text.js"]


export interface RateLimitConfig {
  ttlSeconds: number;
  globalLimit: number;
  messagesLimit: number;
  routesDecideLimit: number;
}

function envInt(name: string, def: number): number {
  const v = process.env[name];
  if (!v) return def;
  const parsed = parseInt(v, 10);
  return Number.isFinite(parsed) ? parsed : def;
}

export const rateLimitConfig: RateLimitConfig = {
  ttlSeconds: envInt('GATEWAY_RATE_LIMIT_TTL_SECONDS', 60),
  globalLimit: envInt('GATEWAY_RATE_LIMIT_GLOBAL_LIMIT', 1000),
  messagesLimit: envInt('GATEWAY_RATE_LIMIT_MESSAGES_LIMIT', 100),
  routesDecideLimit: envInt('GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT', 50),
};

import { registerAs } from '@nestjs/config';

export default registerAs('adminAuth', () => ({
  apiKey: process.env.GATEWAY_ADMIN_API_KEY || 'dev-admin-key',
  rolesAllowlist: (process.env.GATEWAY_ADMIN_ROLES_ALLOWLIST || 'admin').split(',').map((s) => s.trim()),
}));
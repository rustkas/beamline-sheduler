import { CanActivate, ExecutionContext, Injectable, ForbiddenException } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';

@Injectable()
export class AdminApiGuard implements CanActivate {
  constructor(private readonly config: ConfigService) {}

  canActivate(context: ExecutionContext): boolean {
    const req = context.switchToHttp().getRequest();
    const apiKey = req.headers['x-api-key'] as string | undefined;
    const role = (req.headers['x-role'] as string | undefined)?.trim().toLowerCase();
    const expected = this.config.get<string>('adminAuth.apiKey', 'dev-admin-key');
    const roles = this.config.get<string[]>('adminAuth.rolesAllowlist', ['admin']);

    if (!apiKey || apiKey !== expected) {
      throw new ForbiddenException('invalid or missing API key');
    }
    if (!role || !roles.includes(role)) {
      throw new ForbiddenException('invalid or missing role');
    }
    return true;
  }
}
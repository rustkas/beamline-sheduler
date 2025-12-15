import { Injectable } from '@nestjs/common';
import { RouterAdminGrpcService } from './router-admin-grpc.service';
import { UnifiedAuthContext } from './unified-auth-context.service';

export interface RoleMapping {
  router_role: string;
  gateway_permissions: string[];
  description: string;
  priority: number;
}

export interface PermissionMapping {
  router_permission: string;
  gateway_actions: string[];
  resources: string[];
  conditions?: Record<string, any>;
}

export interface ResourceMapping {
  router_resource: string;
  gateway_resources: string[];
  actions: string[];
}

@Injectable()
export class RbacMappingService {
  private roleMappings: RoleMapping[] = [
    {
      router_role: 'admin',
      gateway_permissions: [
        'admin:all',
        'tenant:manage',
        'user:manage',
        'policy:manage',
        'quota:manage',
        'audit:read',
        'system:configure'
      ],
      description: 'Full system administrator',
      priority: 100
    },
    {
      router_role: 'tenant_admin',
      gateway_permissions: [
        'tenant:read',
        'tenant:update',
        'user:manage',
        'policy:manage',
        'quota:read',
        'quota:update',
        'audit:read'
      ],
      description: 'Tenant administrator',
      priority: 90
    },
    {
      router_role: 'developer',
      gateway_permissions: [
        'api:full_access',
        'models:use',
        'flows:manage',
        'blocks:manage',
        'logs:read',
        'metrics:read'
      ],
      description: 'Developer with full API access',
      priority: 80
    },
    {
      router_role: 'user',
      gateway_permissions: [
        'api:limited_access',
        'models:use',
        'flows:read',
        'flows:execute',
        'logs:read_own',
        'metrics:read_own'
      ],
      description: 'Regular user with limited access',
      priority: 70
    },
    {
      router_role: 'viewer',
      gateway_permissions: [
        'api:read_only',
        'models:list',
        'flows:read',
        'logs:read_own',
        'metrics:read_own'
      ],
      description: 'Read-only viewer access',
      priority: 60
    },
    {
      router_role: 'service',
      gateway_permissions: [
        'api:service_access',
        'internal:read',
        'health:read',
        'metrics:read'
      ],
      description: 'Service-to-service communication',
      priority: 50
    },
    {
      router_role: 'anonymous',
      gateway_permissions: [
        'api:public_access',
        'models:list_public',
        'health:read'
      ],
      description: 'Anonymous public access',
      priority: 10
    }
  ];

  private permissionMappings: PermissionMapping[] = [
    {
      router_permission: 'router:admin',
      gateway_actions: ['admin:all', 'system:configure'],
      resources: ['*'],
      conditions: { tenant_admin: true }
    },
    {
      router_permission: 'router:route:read',
      gateway_actions: ['route:read', 'flows:read'],
      resources: ['flows', 'routes', 'policies']
    },
    {
      router_permission: 'router:route:write',
      gateway_actions: ['route:write', 'flows:manage'],
      resources: ['flows', 'routes', 'policies']
    },
    {
      router_permission: 'router:provider:read',
      gateway_actions: ['provider:read', 'models:list'],
      resources: ['providers', 'models']
    },
    {
      router_permission: 'router:provider:write',
      gateway_actions: ['provider:write', 'models:manage'],
      resources: ['providers', 'models']
    },
    {
      router_permission: 'router:quota:read',
      gateway_actions: ['quota:read', 'usage:read'],
      resources: ['quotas', 'usage', 'metrics']
    },
    {
      router_permission: 'router:quota:write',
      gateway_actions: ['quota:write', 'usage:manage'],
      resources: ['quotas', 'usage', 'metrics']
    },
    {
      router_permission: 'router:audit:read',
      gateway_actions: ['audit:read', 'logs:read'],
      resources: ['audit_logs', 'system_logs']
    },
    {
      router_permission: 'router:user:read',
      gateway_actions: ['user:read', 'tenant:read'],
      resources: ['users', 'tenants']
    },
    {
      router_permission: 'router:user:write',
      gateway_actions: ['user:write', 'tenant:manage'],
      resources: ['users', 'tenants']
    },
    {
      router_permission: 'router:policy:read',
      gateway_actions: ['policy:read', 'rules:read'],
      resources: ['policies', 'rules']
    },
    {
      router_permission: 'router:policy:write',
      gateway_actions: ['policy:write', 'rules:manage'],
      resources: ['policies', 'rules']
    }
  ];

  private resourceMappings: ResourceMapping[] = [
    {
      router_resource: 'flows',
      gateway_resources: ['flows', 'routes', 'pipelines'],
      actions: ['read', 'write', 'execute', 'delete']
    },
    {
      router_resource: 'providers',
      gateway_resources: ['providers', 'models', 'ai_services'],
      actions: ['read', 'write', 'configure', 'test']
    },
    {
      router_resource: 'quotas',
      gateway_resources: ['quotas', 'limits', 'usage'],
      actions: ['read', 'write', 'monitor']
    },
    {
      router_resource: 'audit_logs',
      gateway_resources: ['audit_logs', 'system_logs', 'activity_logs'],
      actions: ['read', 'search', 'export']
    },
    {
      router_resource: 'users',
      gateway_resources: ['users', 'accounts', 'profiles'],
      actions: ['read', 'write', 'manage']
    },
    {
      router_resource: 'tenants',
      gateway_resources: ['tenants', 'organizations', 'workspaces'],
      actions: ['read', 'write', 'manage']
    },
    {
      router_resource: 'policies',
      gateway_resources: ['policies', 'rules', 'constraints'],
      actions: ['read', 'write', 'apply']
    }
  ];

  constructor(private readonly routerAdminService: RouterAdminGrpcService) {}

  async mapRouterRolesToGatewayPermissions(routerRoles: string[]): Promise<string[]> {
    const permissions = new Set<string>();
    
    // Sort roles by priority (highest first)
    const sortedRoles = [...this.roleMappings]
      .filter(mapping => routerRoles.includes(mapping.router_role))
      .sort((a, b) => b.priority - a.priority);
    
    // Add permissions from each role
    for (const roleMapping of sortedRoles) {
      roleMapping.gateway_permissions.forEach(permission => permissions.add(permission));
    }
    
    return Array.from(permissions);
  }

  async mapRouterPermissionsToGatewayActions(routerPermissions: string[]): Promise<Map<string, string[]>> {
    const actionMap = new Map<string, string[]>();
    
    for (const routerPermission of routerPermissions) {
      const mapping = this.permissionMappings.find((m) => m.router_permission === routerPermission);
      if (!mapping) continue;
      for (const resource of mapping.resources) {
        const existingActions = actionMap.get(resource) || [];
        actionMap.set(resource, [...new Set([...existingActions, ...mapping.gateway_actions])]);
      }
    }
    
    return actionMap;
  }

  async mapRouterResourceToGatewayResources(routerResource: string): Promise<string[]> {
    const mapping = this.resourceMappings.find(m => m.router_resource === routerResource);
    return mapping ? mapping.gateway_resources : [routerResource];
  }

  async mapGatewayResourceToRouterResource(gatewayResource: string): Promise<string> {
    const mapping = this.resourceMappings.find(m => 
      m.gateway_resources.includes(gatewayResource)
    );
    return mapping ? mapping.router_resource : gatewayResource;
  }

  async checkResourceActionPermission(
    authContext: UnifiedAuthContext,
    resource: string,
    action: string
  ): Promise<boolean> {
    try {
      // Map gateway resource to router resource
      const routerResource = await this.mapGatewayResourceToRouterResource(resource);
      
      // Check if user has required permissions
      const requiredPermissions = this.getRequiredPermissionsForAction(routerResource, action);
      
      return requiredPermissions.every(permission => 
        authContext.user.permissions.includes(permission)
      );
    } catch (error) {
      console.error('Error checking resource action permission:', error);
      return false;
    }
  }

  private getRequiredPermissionsForAction(resource: string, action: string): string[] {
    const permissions: string[] = [];
    
    for (const mapping of this.permissionMappings) {
      if (mapping.resources.includes(resource) && 
          mapping.gateway_actions.some(actionPattern => this.matchesAction(action, actionPattern))) {
        permissions.push(mapping.router_permission);
      }
    }
    
    return permissions;
  }

  private matchesAction(requestedAction: string, permissionAction: string): boolean {
    // Handle wildcard patterns
    if (permissionAction.endsWith(':*')) {
      const baseAction = permissionAction.slice(0, -2);
      return requestedAction.startsWith(baseAction);
    }
    
    // Handle specific action patterns
    if (permissionAction.includes(':')) {
      const [resource, action] = permissionAction.split(':');
      return requestedAction === action;
    }
    
    return requestedAction === permissionAction;
  }

  async validateRoleRequirements(
    authContext: UnifiedAuthContext,
    requiredRoles?: string[],
    requiredPermissions?: string[]
  ): Promise<{ valid: boolean; missing?: string[] }> {
    const missing: string[] = [];
    
    if (requiredRoles && requiredRoles.length > 0) {
      const hasRequiredRole = requiredRoles.some(role => 
        authContext.user.roles.includes(role)
      );
      
      if (!hasRequiredRole) {
        missing.push(`Required roles: ${requiredRoles.join(', ')}`);
      }
    }
    
    if (requiredPermissions && requiredPermissions.length > 0) {
      const missingPermissions = requiredPermissions.filter(permission => 
        !authContext.user.permissions.includes(permission)
      );
      
      if (missingPermissions.length > 0) {
        missing.push(`Required permissions: ${missingPermissions.join(', ')}`);
      }
    }
    
    return {
      valid: missing.length === 0,
      missing: missing.length > 0 ? missing : undefined
    };
  }

  getRoleDescription(role: string): string {
    const mapping = this.roleMappings.find(m => m.router_role === role);
    return mapping ? mapping.description : 'Unknown role';
  }

  getAllRoles(): RoleMapping[] {
    return [...this.roleMappings];
  }

  getAllPermissions(): PermissionMapping[] {
    return [...this.permissionMappings];
  }

  getAllResources(): ResourceMapping[] {
    return [...this.resourceMappings];
  }
}
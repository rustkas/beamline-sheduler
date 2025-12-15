/**
 * Router Admin gRPC Client Interface
 * Defines contract for Router Admin gRPC service communication
 */

import { Observable } from 'rxjs';

/**
 * Admin Policy from Router
 */
export interface AdminPolicy {
  policy_id: string;
  providers: AdminProvider[];
  sticky: boolean;
  rules: AdminRule[];
}

/**
 * Admin Provider in a policy
 */
export interface AdminProvider {
  id: string;
  weight: number;
}

/**
 * Admin Rule in a policy
 */
export interface AdminRule {
  match: string;
  prefer: string[];
  fallback: string;
}

/**
 * Upsert Policy Request
 */
export interface UpsertPolicyRequest {
  tenant_id: string;
  policy: AdminPolicy;
}

/**
 * Upsert Policy Response
 */
export interface UpsertPolicyResponse {
  ok: boolean;
}

/**
 * Delete Policy Request
 */
export interface DeletePolicyRequest {
  tenant_id: string;
  policy_id: string;
}

/**
 * Delete Policy Response
 */
export interface DeletePolicyResponse {
  ok: boolean;
}

/**
 * Get Policy Request
 */
export interface GetPolicyRequest {
  tenant_id: string;
  policy_id: string;
}

/**
 * Get Policy Response
 */
export interface GetPolicyResponse {
  policy: AdminPolicy;
}

/**
 * List Policies Request
 */
export interface ListPoliciesRequest {
  tenant_id: string;
}

/**
 * List Policies Response
 */
export interface ListPoliciesResponse {
  policies: AdminPolicy[];
}

/**
 * Router Admin gRPC Client Interface
 * Matches RouterAdmin service from proto/beamline/flow/v1/flow.proto
 */
export interface IRouterAdminClient {
  /**
   * Upsert a policy (create or update)
   * @param request UpsertPolicyRequest
   * @returns Promise<UpsertPolicyResponse>
   */
  upsertPolicy(request: UpsertPolicyRequest): Promise<UpsertPolicyResponse>;

  /**
   * Delete a policy
   * @param request DeletePolicyRequest
   * @returns Promise<DeletePolicyResponse>
   */
  deletePolicy(request: DeletePolicyRequest): Promise<DeletePolicyResponse>;

  /**
   * Get a policy by ID
   * @param request GetPolicyRequest
   * @returns Promise<GetPolicyResponse>
   */
  getPolicy(request: GetPolicyRequest): Promise<GetPolicyResponse>;

  /**
   * List all policies for a tenant
   * @param request ListPoliciesRequest
   * @returns Promise<ListPoliciesResponse>
   */
  listPolicies(request: ListPoliciesRequest): Promise<ListPoliciesResponse>;

  /**
   * Connect to Router Admin gRPC service
   * @returns Promise<void>
   */
  connect(): Promise<void>;

  /**
   * Disconnect from Router Admin gRPC service
   * @returns Promise<void>
   */
  disconnect(): Promise<void>;

  /**
   * Check if client is connected
   * @returns boolean
   */
  isConnected(): boolean;

  /**
   * Health check for Router Admin connection
   * @returns Promise<boolean>
   */
  healthCheck(): Promise<boolean>;
}


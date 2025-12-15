/**
 * State generator utilities for testing
 * Generates valid and invalid state JSON for testing
 */

export interface StateOptions {
  project?: string;
  version?: string;
  current_cp?: string;
  no_drift?: boolean;
  agents?: any[];
  artifact_checksums?: any[];
}

/**
 * Generate valid state JSON
 */
export function generateValidState(options: StateOptions = {}): any {
  return {
    project: options.project || 'beamline-constructor',
    version: options.version || '1.0',
    current_cp: options.current_cp || 'CP0-LC',
    no_drift: options.no_drift !== undefined ? options.no_drift : true,
    agents: options.agents || [
      {
        id: 'AGENT_1_REPO_STATE',
        name: 'Repo/State Bootstrap',
        task: 'Specify monorepo and project state',
        cp: 'CP0-LC',
        status: 'completed',
        started_at: new Date().toISOString(),
        updated_at: new Date().toISOString()
      }
    ],
    artifact_checksums: options.artifact_checksums || [
      {
        path: 'README.md',
        hash: 'a'.repeat(64),
        ts: new Date().toISOString()
      }
    ],
    updated_at: new Date().toISOString()
  };
}

/**
 * Generate invalid state (missing required fields)
 */
export function generateInvalidState(): any {
  return {
    project: 'beamline-constructor',
    version: '1.0'
    // Missing required fields
  };
}

/**
 * Generate state with no_drift = false
 */
export function generateStateWithDrift(): any {
  return generateValidState({ no_drift: false });
}

/**
 * Generate state with invalid agent status
 */
export function generateStateWithInvalidAgent(): any {
  return generateValidState({
    agents: [
      {
        id: 'AGENT_1_REPO_STATE',
        name: 'Repo/State Bootstrap',
        task: 'Specify monorepo and project state',
        cp: 'CP0-LC',
        status: 'invalid_status', // Invalid status
        started_at: new Date().toISOString(),
        updated_at: new Date().toISOString()
      }
    ]
  });
}

/**
 * Generate state with invalid checksum format
 */
export function generateStateWithInvalidChecksum(): any {
  return generateValidState({
    artifact_checksums: [
      {
        path: 'README.md',
        hash: 'invalid-hash', // Invalid format (not 64 hex chars)
        ts: new Date().toISOString()
      }
    ]
  });
}


import { describe, it, expect } from 'vitest';
import { DevStateHelper } from './helpers/devstate-helper';
import { existsSync, readFileSync } from 'fs';

describe('DevState Debug', () => {
  it('should initialize and read state correctly', async () => {
    const devstate = new DevStateHelper();
    
    // Initialize state
    await devstate.initializeState('test-project');
    
    // Check if files were created
    const statePath = '/home/rustkas/aigroup/tests/.trae/state.json';
    const historyPath = '/home/rustkas/aigroup/tests/.trae/history.json';
    
    console.log('State file exists:', existsSync(statePath));
    console.log('History file exists:', existsSync(historyPath));
    
    if (existsSync(statePath)) {
      const stateContent = readFileSync(statePath, 'utf-8');
      console.log('State content:', stateContent);
      const state = JSON.parse(stateContent);
      console.log('State checksum:', state.checksum);
    }
    
    // Try to read state
    const state = await devstate.readState();
    console.log('Read state successfully:', state.current_cp);
    
    expect(state.project).toBe('test-project');
    expect(state.current_cp).toBe('CP0-LC');
  });
});
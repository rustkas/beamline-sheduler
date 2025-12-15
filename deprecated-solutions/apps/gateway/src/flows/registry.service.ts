import { Injectable } from '@nestjs/common';

export type BlockType = 'http.request' | 'fs.blob_put' | 'fs.blob_get' | 'sql.query';

export interface BlockManifest {
  type: BlockType;
  schema_in?: Record<string, unknown>;
  schema_out?: Record<string, unknown>;
  capabilities?: string[];
}

@Injectable()
export class RegistryService {
  private readonly blocks: Map<BlockType, BlockManifest> = new Map([
    ['http.request', { type: 'http.request', capabilities: ['network'] }],
    ['fs.blob_put', { type: 'fs.blob_put', capabilities: ['storage'] }],
    ['fs.blob_get', { type: 'fs.blob_get', capabilities: ['storage'] }],
    ['sql.query', { type: 'sql.query', capabilities: ['database'] }],
  ]);

  list(): BlockManifest[] {
    return Array.from(this.blocks.values());
  }

  get(type: BlockType): BlockManifest | undefined {
    return this.blocks.get(type);
  }
}

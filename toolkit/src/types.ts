/**
 * Type definitions for @necro-bridge/core
 */

export type LegacyLanguage = 'cobol' | 'fortran' | 'pascal' | 'basic';

export interface BridgeConfig {
  name: string;
  path: string;
  language: LegacyLanguage;
  endpoint: string;
  parameters: string[];
  outputPattern: string;
  timeout?: number;
}

export interface ServerConfig {
  port: number;
  timeout: number;
  cors?: boolean;
}

export interface NecroBridgeConfig {
  binaries: BridgeConfig[];
  server: ServerConfig;
}

export interface ExecutionResult {
  result: any;
  source: string;
  timestamp: string;
  executionTime?: number;
}

export interface ExecutionError {
  error: string;
  details?: string;
  timestamp: string;
}

export interface BinaryInfo {
  path: string;
  language: LegacyLanguage;
  exists: boolean;
  executable: boolean;
}

export interface CompilerInfo {
  name: string;
  command: string;
  installed: boolean;
  version?: string;
}

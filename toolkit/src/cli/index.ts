#!/usr/bin/env node

/**
 * ═══════════════════════════════════════════════════════════
 * NECRO-BRIDGE CLI - COMMAND-LINE INTERFACE
 * ═══════════════════════════════════════════════════════════
 * 
 * UNIVERSAL TOOLKIT FOR LEGACY SYSTEM RESURRECTION
 * PROVIDES COMMANDS FOR INITIALIZATION, DETECTION, SERVING, AND TESTING
 */

import { Command } from 'commander';
import { initCommand } from './commands/init';
import { detectCommand } from './commands/detect';
import { serveCommand } from './commands/serve';
import { testCommand } from './commands/test';

const program = new Command();

program
  .name('necro-bridge')
  .description('🏛️  Universal Bridge Pattern Framework for Legacy System Modernization')
  .version('1.0.0');

// INIT COMMAND - INITIALIZE BRIDGE FOR LEGACY LANGUAGE
program
  .command('init <language>')
  .description('Initialize a bridge for a specific legacy language (cobol, fortran, pascal, basic)')
  .action(initCommand);

// DETECT COMMAND - AUTO-DETECT LEGACY BINARIES
program
  .command('detect')
  .description('Auto-detect legacy binaries in project directory')
  .option('-d, --directory <path>', 'Directory to scan', './legacy')
  .action(detectCommand);

// SERVE COMMAND - START BRIDGE SERVER
program
  .command('serve')
  .description('Start the bridge server')
  .option('-p, --port <number>', 'Server port', '3001')
  .option('-c, --config <path>', 'Config file path', './necro-bridge.config.json')
  .action(serveCommand);

// TEST COMMAND - TEST LEGACY BINARY INTEGRATION
program
  .command('test <binary> [args...]')
  .description('Test a legacy binary integration')
  .action(testCommand);

program.parse(process.argv);

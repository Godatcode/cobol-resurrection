/**
 * ═══════════════════════════════════════════════════════════
 * TEST COMMAND - TEST LEGACY BINARY INTEGRATION
 * ═══════════════════════════════════════════════════════════
 */

import { execSync } from 'child_process';
import * as fs from 'fs';

export async function testCommand(binary: string, args: string[]): Promise<void> {
  console.log('═══════════════════════════════════════════════════════');
  console.log('🧪 TESTING LEGACY BINARY');
  console.log('═══════════════════════════════════════════════════════');
  console.log(`📦 Binary: ${binary}`);
  console.log(`📝 Arguments: ${args.join(' ')}`);
  console.log('');
  
  // CHECK IF BINARY EXISTS
  if (!fs.existsSync(binary)) {
    console.error(`❌ BINARY NOT FOUND: ${binary}`);
    console.error('');
    console.error('SUGGESTIONS:');
    console.error('1. Check the binary path');
    console.error('2. Compile your legacy program first');
    console.error('3. Ensure the binary is executable (chmod +x)');
    process.exit(1);
  }
  
  // CHECK IF BINARY IS EXECUTABLE
  try {
    fs.accessSync(binary, fs.constants.X_OK);
  } catch {
    console.error(`❌ BINARY IS NOT EXECUTABLE: ${binary}`);
    console.error('');
    console.error('FIX:');
    console.error(`   chmod +x ${binary}`);
    process.exit(1);
  }
  
  console.log('🚀 EXECUTING BINARY...');
  console.log('');
  
  try {
    // BUILD COMMAND
    const command = `${binary} ${args.join(' ')}`;
    
    // EXECUTE BINARY
    const startTime = Date.now();
    const output = execSync(command, {
      encoding: 'utf-8',
      stdio: 'pipe',
      timeout: 5000
    });
    const duration = Date.now() - startTime;
    
    // DISPLAY OUTPUT
    console.log('═══════════════════════════════════════════════════════');
    console.log('📤 OUTPUT:');
    console.log('═══════════════════════════════════════════════════════');
    console.log(output);
    console.log('═══════════════════════════════════════════════════════');
    console.log('');
    
    // PARSE OUTPUT
    const resultMatch = output.match(/RESULT:\s*(\d+\.?\d*)/);
    if (resultMatch) {
      console.log(`✅ RESULT DETECTED: ${resultMatch[1]}`);
    } else {
      console.log('⚠️  NO RESULT PATTERN DETECTED');
      console.log('   Expected format: "RESULT: <value>"');
    }
    
    console.log('');
    console.log(`⏱️  Execution time: ${duration}ms`);
    console.log('');
    console.log('═══════════════════════════════════════════════════════');
    console.log('✅ TEST SUCCESSFUL');
    console.log('═══════════════════════════════════════════════════════');
    console.log('');
    console.log('NEXT STEPS:');
    console.log('1. Start the bridge server: necro-bridge serve');
    console.log('2. Test via API:');
    console.log('   curl -X POST http://localhost:3001/api/calculate \\');
    console.log('     -H "Content-Type: application/json" \\');
    console.log(`     -d '{"param1": ${args[0] || 0}, "param2": ${args[1] || 0}, "param3": ${args[2] || 0}}'`);
    console.log('');
    
  } catch (error: any) {
    console.log('═══════════════════════════════════════════════════════');
    console.log('❌ EXECUTION FAILED');
    console.log('═══════════════════════════════════════════════════════');
    console.log('');
    
    if (error.code === 'ETIMEDOUT') {
      console.error('⏱️  TIMEOUT: Binary took longer than 5 seconds');
    } else if (error.status) {
      console.error(`🔴 EXIT CODE: ${error.status}`);
    }
    
    if (error.stderr) {
      console.error('');
      console.error('📤 STDERR:');
      console.error(error.stderr);
    }
    
    if (error.stdout) {
      console.error('');
      console.error('📤 STDOUT:');
      console.error(error.stdout);
    }
    
    console.error('');
    console.error('TROUBLESHOOTING:');
    console.error('1. Check if the binary accepts command-line arguments');
    console.error('2. Verify the argument format matches your program');
    console.error('3. Test the binary directly: ' + `${binary} ${args.join(' ')}`);
    console.error('4. Check for compilation errors');
    console.error('');
    
    process.exit(1);
  }
}

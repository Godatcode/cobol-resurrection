/**
 * API INTEGRATION TEST FOR CODE GENERATION
 * Tests the /api/generate endpoint
 */

const http = require('http');

console.log('═══════════════════════════════════════════════════════');
console.log('🧪 API CODE GENERATION INTEGRATION TEST');
console.log('═══════════════════════════════════════════════════════\n');

// Test COBOL code
const testCobol = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. APITEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VALUE PIC 9(4)V99 VALUE 555.55.
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "RESULT: " WS-VALUE
           STOP RUN.`;

const requestData = JSON.stringify({
  code: testCobol,
  language: 'COBOL',
  filename: 'api-test-gen'
});

const options = {
  hostname: 'localhost',
  port: 3001,
  path: '/api/generate',
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
    'Content-Length': Buffer.byteLength(requestData)
  }
};

console.log('📡 Sending POST request to /api/generate...\n');

const req = http.request(options, (res) => {
  let data = '';
  
  res.on('data', (chunk) => {
    data += chunk;
  });
  
  res.on('end', () => {
    console.log(`Status Code: ${res.statusCode}`);
    console.log('Response:');
    
    try {
      const response = JSON.parse(data);
      console.log(JSON.stringify(response, null, 2));
      
      if (response.success) {
        console.log('\n✓ CODE GENERATION SUCCESSFUL');
        console.log(`✓ File saved: ${response.file_path}`);
        console.log(`✓ Compilation: ${response.compilation.success ? 'SUCCESS' : 'FAILED'}`);
      } else {
        console.log('\n✗ CODE GENERATION FAILED');
        console.log(`Error: ${response.error}`);
      }
    } catch (error) {
      console.log(data);
    }
    
    console.log('\n═══════════════════════════════════════════════════════');
  });
});

req.on('error', (error) => {
  console.log('✗ REQUEST FAILED');
  console.log(`Error: ${error.message}`);
  console.log('\nNote: Make sure the server is running on port 3001');
  console.log('Run: node server/server.js');
  console.log('\n═══════════════════════════════════════════════════════');
});

req.write(requestData);
req.end();

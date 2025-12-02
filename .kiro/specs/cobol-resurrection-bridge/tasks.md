# Implementation Plan

- [x] 1. Set up project structure and Kiro configurations
  - Create directory structure: /legacy, /server, /client, /.kiro
  - Create .kiro/hooks/compile.yaml with COBOL auto-compilation configuration
  - Create .kiro/mcp/server.json with run_legacy_calc tool definition
  - Create .kiro/steering/persona.md with IBM 7090 Mainframe persona
  - _Requirements: 7.1, 7.5, 4.2, 4.3, 5.1, 6.4_

- [x] 2. Implement COBOL mortgage calculator
  - Write mortgage.cbl with GnuCOBOL-compatible syntax
  - Implement command-line argument parsing for principal, rate, and term
  - Implement mortgage calculation using formula M = P [ i(1 + i)^n ] / [ (1 + i)^n – 1 ]
  - Implement STDOUT output in format "RESULT: XXXX.XX"
  - Implement input validation and error handling with STDERR messages
  - _Requirements: 1.1, 1.2, 1.3, 1.4_

- [x] 2.1 Write property test for COBOL calculation accuracy
  - **Property 1: COBOL Mortgage Calculation Accuracy**
  - **Validates: Requirements 1.2**

- [x] 2.2 Write property test for COBOL output format
  - **Property 2: COBOL Output Format Consistency**
  - **Validates: Requirements 1.3**

- [x] 2.3 Write property test for COBOL error handling
  - **Property 3: COBOL Error Handling**
  - **Validates: Requirements 1.4**

- [x] 2.4 Write unit tests for COBOL edge cases
  - Test specific examples: $200,000 at 5.5% for 30 years
  - Test edge cases: $1 principal, 29.99% rate, 1 year term
  - Test error messages for invalid inputs
  - _Requirements: 1.2, 1.4_

- [x] 3. Initialize Node.js Bridge Server
  - Create /server directory with package.json
  - Install dependencies: express, cors
  - Create server.js with Express app setup
  - Configure CORS for React client
  - Set up server to listen on port 3001
  - _Requirements: 2.1, 7.5_

- [x] 4. Implement Bridge Server API endpoint
  - Create POST /api/calculate route handler
  - Implement input validation for principal, rate, and term
  - Implement child_process.exec() to spawn COBOL binary
  - Implement timeout handling (5 second limit)
  - Capture STDOUT and STDERR from COBOL process
  - _Requirements: 2.1, 2.2_

- [x] 4.1 Implement Bridge Server output parsing
  - Create regex pattern to extract payment from "RESULT: XXXX.XX"
  - Transform parsed value to JSON response format
  - Return { "monthly_payment": number, "source": "COBOL_LEGACY_ENGINE" }
  - _Requirements: 2.3, 2.4_

- [x] 4.2 Implement Bridge Server error handling
  - Handle COBOL binary not found errors
  - Handle non-zero exit codes
  - Handle parsing failures
  - Return HTTP 500 with "CORE DUMP DETECTED" message
  - Return HTTP 400 for input validation errors
  - _Requirements: 2.5_

- [x] 4.3 Write property test for Bridge Server process spawning
  - **Property 4: Bridge Server Process Spawning**
  - **Validates: Requirements 2.1, 2.2**

- [x] 4.4 Write property test for Bridge Server response transformation
  - **Property 5: Bridge Server Response Transformation**
  - **Validates: Requirements 2.3, 2.4**

- [x] 4.5 Write property test for Bridge Server error response
  - **Property 6: Bridge Server Error Response**
  - **Validates: Requirements 2.5**

- [x] 4.6 Write unit tests for Bridge Server
  - Test API endpoint routing
  - Test input validation logic
  - Test regex parsing with various RESULT formats
  - Test timeout handling
  - _Requirements: 2.1, 2.2, 2.3, 2.4, 2.5_

- [x] 5. Initialize React Necro-Bank UI
  - Create /client directory with Vite + React
  - Install dependencies: react, vite, tailwindcss, axios
  - Configure Tailwind with custom colors (#000000, #00ff00)
  - Configure custom font family (Courier New, monospace)
  - Set up proxy or CORS for API calls to localhost:3001
  - _Requirements: 3.1, 3.2, 7.5_

- [x] 6. Implement UI Header component
  - Create Header component with title "🏦 NECRO-BANK SYSTEM v1.0"
  - Apply mainframe-green text color (#00ff00)
  - Apply Courier New monospace font
  - _Requirements: 3.1_

- [x] 7. Implement MortgageForm component
  - Create form with three input fields: Principal, Annual Rate, Term (Years)
  - Implement input validation (positive numbers, required fields)
  - Create Calculate button with haunted mainframe styling
  - Implement form submission handler that calls /api/calculate
  - Apply green border styling with black background
  - _Requirements: 3.3_

- [x] 8. Implement TerminalWindow component
  - Create scrollable log display container
  - Apply black background (#000000) and green text (#00ff00)
  - Apply Courier New monospace font
  - Implement log entry rendering with timestamps
  - Style with retro terminal aesthetic (optional: scanline effect)
  - _Requirements: 3.4, 3.5_

- [x] 8.1 Implement terminal boot sequence animation
  - Create boot sequence array with messages and delays
  - Implement sequential message display: "INITIALIZING TAPE DRIVE..." (0ms), "MOUNTING VOLUME..." (400ms), "LOADING COBOL RUNTIME..." (800ms), "EXECUTING LEGACY SUBROUTINE..." (1200ms)
  - Trigger animation on Calculate button click
  - Display result message after boot sequence completes
  - _Requirements: 3.4, 8.1_

- [x] 8.2 Implement terminal result display
  - Parse API response and extract monthly_payment
  - Format result message: "RESULT: Monthly Payment = $XXXX.XX"
  - Append result to terminal log after boot sequence
  - Handle error responses with error message display
  - _Requirements: 3.5, 8.4_

- [x] 8.3 Implement terminal log management
  - Decide on log strategy: append or clear-and-restart
  - Implement chosen strategy consistently
  - Ensure logs remain readable across multiple calculations
  - _Requirements: 8.5_

- [x] 8.4 Write property test for UI font consistency
  - **Property 7: UI Font Consistency**
  - **Validates: Requirements 3.2**

- [x] 8.5 Write property test for terminal boot sequence
  - **Property 8: Terminal Boot Sequence**
  - **Validates: Requirements 3.4, 8.1, 8.3**

- [x] 8.6 Write property test for terminal result display
  - **Property 9: Terminal Result Display**
  - **Validates: Requirements 3.5, 8.4**

- [x] 8.7 Write property test for terminal log management
  - **Property 10: Terminal Log Management**
  - **Validates: Requirements 8.5**

- [x] 8.8 Write unit tests for UI components
  - Test form input validation
  - Test button click handlers
  - Test API integration with mocked responses
  - Test terminal log rendering
  - _Requirements: 3.3, 3.4, 3.5_

- [x] 9. Implement MCP tool functionality
  - Verify .kiro/mcp/server.json configuration is correct
  - Test run_legacy_calc tool manually with sample parameters
  - Verify tool executes COBOL binary and returns parsed result
  - Verify tool handles errors appropriately
  - _Requirements: 5.2, 5.3, 5.4_

- [x] 9.1 Write property test for MCP tool execution
  - **Property 11: MCP Tool Execution**
  - **Validates: Requirements 5.2, 5.3**

- [x] 9.2 Write property test for MCP tool error handling
  - **Property 12: MCP Tool Error Handling**
  - **Validates: Requirements 5.4**

- [x] 10. Final integration and testing checkpoint
  - Ensure all tests pass, ask the user if questions arise
  - Verify COBOL compiles successfully
  - Verify Bridge Server starts and responds to requests
  - Verify React UI loads and displays correctly
  - Test complete end-to-end flow: form submission → API call → COBOL execution → result display
  - Verify agent hook triggers on .cbl file save
  - Verify MCP tool works from AI chat
  - Verify steering persona is applied
  - _Requirements: All_

- [x] 11. Create README documentation
  - Document prerequisites (GnuCOBOL, Node.js)
  - Document setup steps (compilation, installation, running)
  - Document project structure
  - Document API endpoints
  - Document Kiro integrations (hooks, MCP, steering)
  - Include demo script for hackathon presentation
  - _Requirements: All_

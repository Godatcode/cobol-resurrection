# Requirements Document

## Introduction

The COBOL Resurrection Bridge is a hackathon project that demonstrates "cyber-necromancy" by resurrecting ancient COBOL banking code within a modern Node.js and React environment. The system allows users to calculate mortgage payments using a legacy COBOL engine through a haunted mainframe-themed web interface, showcasing the integration of 1960s-era programming with contemporary web technologies.

## Glossary

- **Legacy Engine**: The compiled COBOL binary that performs mortgage calculations
- **Bridge Server**: The Node.js Express API that spawns and communicates with the Legacy Engine
- **Necro-Bank UI**: The React frontend application with haunted mainframe aesthetics
- **COBOL Binary**: The executable file compiled from mortgage.cbl using GnuCOBOL
- **Terminal Window**: The UI component that displays boot-up logs and calculation results
- **Agent Hook**: A Kiro automation that triggers on file save events
- **MCP Tool**: A Model Context Protocol tool that enables AI interaction with the Legacy Engine

## Requirements

### Requirement 1

**User Story:** As a hackathon judge, I want to see a working COBOL program that calculates mortgage payments, so that I can verify the project resurrects genuine legacy code.

#### Acceptance Criteria

1. WHEN the mortgage.cbl file is compiled THEN the system SHALL produce a GnuCOBOL-compatible executable binary
2. WHEN the COBOL Binary receives three command-line arguments (principal, annual interest rate, loan term in years) THEN the Legacy Engine SHALL calculate the monthly mortgage payment using the formula M = P [ i(1 + i)^n ] / [ (1 + i)^n – 1 ]
3. WHEN the calculation completes THEN the Legacy Engine SHALL print the result to STDOUT in the format "RESULT: 0000.00"
4. WHEN invalid inputs are provided THEN the Legacy Engine SHALL handle errors gracefully without crashing
5. WHEN the COBOL Binary executes THEN the system SHALL complete the calculation within 1 second for typical mortgage values

### Requirement 2

**User Story:** As a web application user, I want to submit mortgage parameters through an API, so that I can receive calculated payments without directly interacting with COBOL.

#### Acceptance Criteria

1. WHEN a POST request is sent to /api/calculate with JSON payload containing principal, rate, and term THEN the Bridge Server SHALL spawn a shell process to execute the COBOL Binary
2. WHEN the COBOL Binary outputs to STDOUT THEN the Bridge Server SHALL capture the output stream
3. WHEN the Bridge Server receives "RESULT: 0000.00" format THEN the system SHALL extract the numeric value using regex pattern matching
4. WHEN the extraction succeeds THEN the Bridge Server SHALL return JSON response with structure { "monthly_payment": 123.45, "source": "COBOL_LEGACY_ENGINE" }
5. IF the COBOL Binary fails or returns non-zero exit code THEN the Bridge Server SHALL return HTTP 500 status with message "CORE DUMP DETECTED"

### Requirement 3

**User Story:** As a hackathon attendee, I want to interact with a haunted mainframe-themed interface, so that I can experience the "cyber-necromancy" aesthetic while calculating mortgages.

#### Acceptance Criteria

1. WHEN the Necro-Bank UI loads THEN the system SHALL display a title "🏦 NECRO-BANK SYSTEM v1.0" in bright green text (#00ff00) on black background (#000000)
2. WHEN the user views the interface THEN the system SHALL render all text using monospaced Courier New font family
3. WHEN the user interacts with the form THEN the system SHALL provide input fields for Principal, Annual Interest Rate, and Loan Term in Years
4. WHEN the user clicks the Calculate button THEN the Terminal Window SHALL display sequential boot-up logs including "INITIALIZING TAPE DRIVE..." and "MOUNTING VOLUME..." before showing results
5. WHEN the calculation completes THEN the Terminal Window SHALL append the monthly payment result to the log display

### Requirement 4

**User Story:** As a developer, I want automatic COBOL compilation on file save, so that I can iterate quickly without manual build steps.

#### Acceptance Criteria

1. WHEN any file matching pattern `**/*.cbl` is saved THEN the system SHALL trigger the Agent Hook automatically
2. WHEN the Agent Hook executes THEN the system SHALL run the command `cobc -x -o legacy/mortgage legacy/mortgage.cbl`
3. WHEN the compilation succeeds THEN the system SHALL display message "👻 Ancient Spirit Rebound to Binary."
4. WHEN the compilation fails THEN the system SHALL report the compilation errors to the user
5. WHEN multiple .cbl files are saved in rapid succession THEN the system SHALL queue compilation tasks without conflicts

### Requirement 5

**User Story:** As an AI assistant, I want to invoke the COBOL binary directly through MCP tools, so that I can test the legacy engine during development conversations.

#### Acceptance Criteria

1. WHEN the MCP server configuration is loaded THEN the system SHALL register a tool named `run_legacy_calc`
2. WHEN the run_legacy_calc tool is invoked with principal, rate, and term parameters THEN the system SHALL execute the COBOL Binary with those arguments
3. WHEN the COBOL Binary completes THEN the MCP Tool SHALL return the parsed monthly payment value
4. WHEN the COBOL Binary fails THEN the MCP Tool SHALL return an error message with diagnostic information
5. WHEN the tool is called multiple times THEN the system SHALL handle concurrent executions without race conditions

### Requirement 6

**User Story:** As a hackathon judge evaluating personality, I want the AI to adopt a mainframe persona, so that the project demonstrates creative integration of Kiro steering capabilities.

#### Acceptance Criteria

1. WHEN the AI responds in the context of this project THEN the system SHALL use uppercase letters preferentially
2. WHEN the AI refers to software defects THEN the system SHALL use the term "moths" instead of "bugs"
3. WHEN the AI completes a message THEN the system SHALL append "[END OF TAPE]" to the response
4. WHEN the steering file is present THEN the system SHALL apply the IBM 7090 Mainframe persona consistently
5. WHEN users interact with the AI THEN the system SHALL maintain the persona without breaking character

### Requirement 7

**User Story:** As a project maintainer, I want clear separation between legacy, server, and client code, so that the architecture demonstrates proper resurrection bridge patterns.

#### Acceptance Criteria

1. WHEN the project structure is created THEN the system SHALL organize code into /legacy, /server, and /client directories
2. WHEN the Legacy Engine is modified THEN the Bridge Server and Necro-Bank UI SHALL remain unaffected
3. WHEN the Bridge Server API changes THEN the Legacy Engine SHALL continue functioning independently
4. WHEN the Necro-Bank UI is updated THEN the Bridge Server and Legacy Engine SHALL operate without modification
5. WHEN dependencies are installed THEN the system SHALL maintain separate package.json files for server and client

### Requirement 8

**User Story:** As a user, I want to see realistic terminal animations, so that the experience feels like operating vintage computing equipment.

#### Acceptance Criteria

1. WHEN the Calculate button is clicked THEN the Terminal Window SHALL display boot-up messages with sequential timing delays
2. WHEN boot-up logs appear THEN the system SHALL simulate typewriter effect or instant line-by-line rendering
3. WHEN the calculation is in progress THEN the Terminal Window SHALL show at least 3 distinct status messages
4. WHEN the result is ready THEN the Terminal Window SHALL display the final monthly payment in the log sequence
5. WHEN multiple calculations are performed THEN the Terminal Window SHALL append new logs while preserving previous entries or clear and restart the sequence

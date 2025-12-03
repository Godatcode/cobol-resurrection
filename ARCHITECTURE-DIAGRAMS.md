# 🏛️ ARCHITECTURE DIAGRAMS

VISUAL REPRESENTATIONS OF THE COBOL RESURRECTION BRIDGE SYSTEM ARCHITECTURE

---

## 📊 SYSTEM OVERVIEW

```mermaid
graph TB
    subgraph "PRESENTATION LAYER"
        UI[React UI<br/>Necro-Bank Interface]
        PC[Punch Card Visualizer]
        TR[Tape Reel Animator]
        PL[Panel Lights Display]
        MD[Memory Dump]
        AM[Audio Manager]
    end
    
    subgraph "API LAYER"
        API[Express Server<br/>Port 3001]
        BF[Bridge Factory]
        CB[COBOL Bridge]
        FB[FORTRAN Bridge]
        PB[PASCAL Bridge]
        BB[BASIC Bridge]
    end
    
    subgraph "LEGACY LAYER"
        C[COBOL Binary<br/>1959]
        F[FORTRAN Binary<br/>1957]
        P[PASCAL Binary<br/>1970]
        B[BASIC Binary<br/>1983]
    end
    
    subgraph "KIRO INTEGRATION"
        AH[Agent Hooks<br/>Auto-Compile]
        MCP[MCP Tools<br/>AI Testing]
        ST[Steering<br/>Persona]
        CG[Code Generator<br/>GPT-4]
    end
    
    UI --> API
    API --> BF
    BF --> CB
    BF --> FB
    BF --> PB
    BF --> BB
    CB --> C
    FB --> F
    PB --> P
    BB --> B
    
    AH -.-> C
    AH -.-> F
    AH -.-> P
    AH -.-> B
    
    MCP -.-> C
    MCP -.-> F
    MCP -.-> P
    MCP -.-> B
    
    CG -.-> C
    CG -.-> F
    CG -.-> P
    CG -.-> B
    
    style UI fill:#00ff00,stroke:#00ff00,color:#000
    style API fill:#ffb000,stroke:#ffb000,color:#000
    style C fill:#ff0000,stroke:#ff0000,color:#fff
    style F fill:#ff0000,stroke:#ff0000,color:#fff
    style P fill:#ff0000,stroke:#ff0000,color:#fff
    style B fill:#ff0000,stroke:#ff0000,color:#fff
```

---

## 🔄 DATA FLOW - CALCULATION REQUEST

```mermaid
sequenceDiagram
    participant User
    participant UI as React UI
    participant API as Bridge Server
    participant Bridge as Language Bridge
    participant Binary as Legacy Binary
    
    User->>UI: Enter parameters & click Calculate
    UI->>UI: Validate inputs
    UI->>UI: Start boot sequence animation
    UI->>API: POST /api/calculate/{language}
    
    API->>API: Validate request
    API->>Bridge: BridgeFactory.createBridge(language)
    Bridge->>Bridge: Validate parameters
    Bridge->>Binary: spawn process with args
    
    Binary->>Binary: Parse arguments
    Binary->>Binary: Validate inputs
    Binary->>Binary: Perform calculation
    Binary->>Bridge: STDOUT: "RESULT: XXXX.XX"
    
    Bridge->>Bridge: Parse output (regex)
    Bridge->>API: Return result object
    API->>UI: JSON response
    
    UI->>UI: Complete boot sequence
    UI->>UI: Display result
    UI->>User: Show monthly payment
```

---

## 🤖 AI CODE GENERATION FLOW

```mermaid
sequenceDiagram
    participant User
    participant UI as Code Generator Modal
    participant API as Bridge Server
    participant CG as Code Generator Service
    participant GPT as GPT-4 API
    participant FS as File System
    participant AH as Agent Hook
    participant Compiler
    
    User->>UI: Click "Summon Ancient Spirit"
    User->>UI: Enter description & select language
    UI->>API: POST /api/generate-code
    
    API->>CG: Generate code request
    CG->>CG: Build prompt with examples
    CG->>GPT: Send prompt
    GPT->>CG: Return generated code
    
    CG->>CG: Validate syntax
    CG->>FS: Save to legacy/{language}/
    FS->>AH: File save event
    
    AH->>Compiler: Trigger compilation
    Compiler->>AH: Compilation result
    AH->>UI: Status update
    
    UI->>User: Display generated code
    UI->>User: Show compilation status
```

---

## 🏗️ BRIDGE PATTERN ARCHITECTURE

```mermaid
classDiagram
    class LegacyBridge {
        <<abstract>>
        +String binaryPath
        +String language
        +Number year
        +execute(params) Promise
        +validateParams(params) Boolean
        +formatArgs(params) Array
        +parseOutput(stdout) Number
        +handleError(error) Object
    }
    
    class CobolBridge {
        +validateParams(params)
        +formatArgs(params)
    }
    
    class FortranBridge {
        +validateParams(params)
        +formatArgs(params)
    }
    
    class PascalBridge {
        +validateParams(params)
        +formatArgs(params)
    }
    
    class BasicBridge {
        +validateParams(params)
        +formatArgs(params)
    }
    
    class BridgeFactory {
        +createBridge(language) LegacyBridge
    }
    
    LegacyBridge <|-- CobolBridge
    LegacyBridge <|-- FortranBridge
    LegacyBridge <|-- PascalBridge
    LegacyBridge <|-- BasicBridge
    BridgeFactory ..> LegacyBridge : creates
```

---

## 📦 COMPONENT HIERARCHY

```mermaid
graph TD
    App[App.jsx]
    App --> Header[Header.jsx]
    App --> LangSelect[Language Selector]
    App --> Form[MortgageForm.jsx]
    App --> Terminal[TerminalWindow.jsx]
    App --> Museum[Museum Components]
    
    Museum --> PunchCard[PunchCard.tsx]
    Museum --> TapeReel[TapeReel.tsx]
    Museum --> PanelLights[PanelLights.tsx]
    Museum --> MemoryDump[MemoryDump.tsx]
    Museum --> VolumeControl[VolumeControl.tsx]
    
    App --> CodeGen[CodeGeneratorModal.jsx]
    
    App --> AudioMgr[AudioManager.ts]
    
    style App fill:#00ff00,stroke:#00ff00,color:#000
    style Museum fill:#ffb000,stroke:#ffb000,color:#000
    style AudioMgr fill:#ff0000,stroke:#ff0000,color:#fff
```

---

## 🗂️ DIRECTORY STRUCTURE

```mermaid
graph LR
    Root[cobol-resurrection-bridge/]
    
    Root --> Legacy[legacy/]
    Root --> Server[server/]
    Root --> Client[client/]
    Root --> Tests[tests/]
    Root --> Toolkit[toolkit/]
    Root --> Kiro[.kiro/]
    
    Legacy --> COBOL[cobol/]
    Legacy --> FORTRAN[fortran/]
    Legacy --> PASCAL[pascal/]
    Legacy --> BASIC[basic/]
    
    Server --> Bridges[bridges/]
    Server --> Services[services/]
    Server --> Utils[utils/]
    
    Client --> Src[src/]
    Src --> Components[components/]
    Src --> ServicesC[services/]
    Src --> Styles[styles/]
    
    Kiro --> Hooks[hooks/]
    Kiro --> MCP[mcp/]
    Kiro --> Steering[steering/]
    
    Toolkit --> CLI[cli/]
    Toolkit --> Templates[templates/]
    
    style Root fill:#00ff00,stroke:#00ff00,color:#000
    style Legacy fill:#ff0000,stroke:#ff0000,color:#fff
    style Server fill:#ffb000,stroke:#ffb000,color:#000
    style Client fill:#00ff00,stroke:#00ff00,color:#000
```

---

## 🔌 API ENDPOINT ROUTING

```mermaid
graph LR
    Client[Client Request]
    
    Client --> Health[GET /api/health]
    Client --> Languages[GET /api/languages]
    Client --> CalcCOBOL[POST /api/calculate/cobol]
    Client --> CalcFORTRAN[POST /api/calculate/fortran]
    Client --> CalcPASCAL[POST /api/calculate/pascal]
    Client --> CalcBASIC[POST /api/calculate/basic]
    Client --> GenCode[POST /api/generate-code]
    Client --> Legacy[POST /api/calculate]
    
    Health --> Response[JSON Response]
    Languages --> Response
    CalcCOBOL --> BridgeCOBOL[COBOL Bridge]
    CalcFORTRAN --> BridgeFORTRAN[FORTRAN Bridge]
    CalcPASCAL --> BridgePASCAL[PASCAL Bridge]
    CalcBASIC --> BridgeBASIC[BASIC Bridge]
    GenCode --> CodeGenService[Code Generator]
    Legacy --> BridgeCOBOL
    
    BridgeCOBOL --> Response
    BridgeFORTRAN --> Response
    BridgePASCAL --> Response
    BridgeBASIC --> Response
    CodeGenService --> Response
    
    style Client fill:#00ff00,stroke:#00ff00,color:#000
    style Response fill:#ffb000,stroke:#ffb000,color:#000
```

---

## 🧪 TESTING ARCHITECTURE

```mermaid
graph TB
    subgraph "PROPERTY-BASED TESTS"
        PBT1[Calculation Accuracy]
        PBT2[Output Format]
        PBT3[Error Handling]
        PBT4[Bridge Behavior]
        PBT5[UI Properties]
    end
    
    subgraph "UNIT TESTS"
        UT1[Component Tests]
        UT2[Bridge Tests]
        UT3[API Tests]
        UT4[Utility Tests]
    end
    
    subgraph "INTEGRATION TESTS"
        IT1[End-to-End Flow]
        IT2[Multi-Language]
        IT3[Error Scenarios]
        IT4[Code Generation]
    end
    
    subgraph "TEST FRAMEWORK"
        Vitest[Vitest]
        FastCheck[fast-check]
        TestingLib[Testing Library]
    end
    
    PBT1 --> FastCheck
    PBT2 --> FastCheck
    PBT3 --> FastCheck
    PBT4 --> FastCheck
    PBT5 --> FastCheck
    
    UT1 --> TestingLib
    UT2 --> Vitest
    UT3 --> Vitest
    UT4 --> Vitest
    
    IT1 --> Vitest
    IT2 --> Vitest
    IT3 --> Vitest
    IT4 --> Vitest
    
    style FastCheck fill:#00ff00,stroke:#00ff00,color:#000
    style Vitest fill:#ffb000,stroke:#ffb000,color:#000
```

---

## 🚀 DEPLOYMENT ARCHITECTURE

```mermaid
graph TB
    subgraph "DEVELOPMENT"
        DevUI[Vite Dev Server<br/>:5173]
        DevAPI[Node Server<br/>:3001]
        DevBin[Local Binaries]
    end
    
    subgraph "PRODUCTION"
        CDN[CDN<br/>Vercel/Netlify]
        API[API Server<br/>Railway/Render]
        Docker[Docker Container]
    end
    
    subgraph "DOCKER CONTAINER"
        Node[Node.js 18]
        Compilers[Legacy Compilers]
        Binaries[Compiled Binaries]
        Express[Express Server]
    end
    
    DevUI -.-> DevAPI
    DevAPI -.-> DevBin
    
    CDN --> API
    API --> Docker
    Docker --> Node
    Docker --> Compilers
    Docker --> Binaries
    Docker --> Express
    
    style DevUI fill:#00ff00,stroke:#00ff00,color:#000
    style CDN fill:#00ff00,stroke:#00ff00,color:#000
    style Docker fill:#ff0000,stroke:#ff0000,color:#fff
```

---

## 🔐 SECURITY LAYERS

```mermaid
graph TB
    Request[Client Request]
    
    Request --> ClientVal[Client-Side Validation]
    ClientVal --> ServerVal[Server-Side Validation]
    ServerVal --> Sanitize[Input Sanitization]
    Sanitize --> Spawn[Process Spawning]
    
    Spawn --> Timeout[5s Timeout]
    Spawn --> Sandbox[Process Isolation]
    Spawn --> NoFS[No File System Access]
    Spawn --> NoNet[No Network Access]
    
    Timeout --> Response[Response]
    Sandbox --> Response
    NoFS --> Response
    NoNet --> Response
    
    Response --> ErrorFilter[Error Message Filtering]
    ErrorFilter --> Client[Client Response]
    
    style Request fill:#00ff00,stroke:#00ff00,color:#000
    style Spawn fill:#ff0000,stroke:#ff0000,color:#fff
    style Client fill:#00ff00,stroke:#00ff00,color:#000
```

---

## 📊 PERFORMANCE CHARACTERISTICS

```mermaid
graph LR
    subgraph "REQUEST LATENCY"
        Network1[Network: 10ms]
        Validation[Validation: 1ms]
        Spawn[Process Spawn: 50ms]
        Execution[Calculation: 5ms]
        Parsing[Parsing: 1ms]
        Network2[Network: 10ms]
    end
    
    Network1 --> Validation
    Validation --> Spawn
    Spawn --> Execution
    Execution --> Parsing
    Parsing --> Network2
    
    Network2 --> Total[Total: ~78ms]
    
    style Total fill:#00ff00,stroke:#00ff00,color:#000
```

---

## 🎯 KIRO INTEGRATION POINTS

```mermaid
graph TB
    subgraph "KIRO IDE"
        Editor[Code Editor]
        Chat[AI Chat]
        Terminal[Terminal]
    end
    
    subgraph "AGENT HOOKS"
        FileSave[File Save Event]
        Compile[Auto-Compile]
        Notify[Notification]
    end
    
    subgraph "MCP TOOLS"
        RunCalc[run_legacy_calc]
        SummonSpirit[summon_ancient_spirit]
    end
    
    subgraph "STEERING"
        Persona[IBM 7090 Persona]
    end
    
    Editor --> FileSave
    FileSave --> Compile
    Compile --> Notify
    
    Chat --> RunCalc
    Chat --> SummonSpirit
    Chat --> Persona
    
    RunCalc --> Binary[Legacy Binary]
    SummonSpirit --> CodeGen[Code Generator]
    
    style Editor fill:#00ff00,stroke:#00ff00,color:#000
    style Chat fill:#ffb000,stroke:#ffb000,color:#000
    style Binary fill:#ff0000,stroke:#ff0000,color:#fff
```

---

## 🔄 TOOLKIT CLI WORKFLOW

```mermaid
graph TB
    User[Developer]
    
    User --> Setup[necro-bridge setup]
    User --> Init[necro-bridge init]
    User --> Detect[necro-bridge detect]
    User --> Serve[necro-bridge serve]
    User --> Test[necro-bridge test]
    
    Setup --> Wizard[Interactive Wizard]
    Wizard --> Config[Generate Config]
    
    Init --> Template[Load Template]
    Template --> Generate[Generate Files]
    
    Detect --> Scan[Scan Directories]
    Scan --> FindBin[Find Binaries]
    FindBin --> FindComp[Check Compilers]
    FindComp --> Config
    
    Serve --> LoadConfig[Load Config]
    LoadConfig --> StartServer[Start Server]
    
    Test --> RunBinary[Execute Binary]
    RunBinary --> ValidateOutput[Validate Output]
    
    style User fill:#00ff00,stroke:#00ff00,color:#000
    style Config fill:#ffb000,stroke:#ffb000,color:#000
```

---

## 📈 SCALABILITY MODEL

```mermaid
graph TB
    subgraph "SINGLE INSTANCE"
        NodeJS[Node.js Event Loop]
        Process1[Process 1]
        Process2[Process 2]
        ProcessN[Process N]
    end
    
    subgraph "HORIZONTAL SCALING"
        LB[Load Balancer]
        Instance1[Instance 1]
        Instance2[Instance 2]
        InstanceN[Instance N]
    end
    
    subgraph "OPTIMIZATION"
        Cache[Result Caching]
        Pool[Process Pooling]
        CDN[Static Asset CDN]
    end
    
    NodeJS --> Process1
    NodeJS --> Process2
    NodeJS --> ProcessN
    
    LB --> Instance1
    LB --> Instance2
    LB --> InstanceN
    
    Cache -.-> NodeJS
    Pool -.-> NodeJS
    CDN -.-> LB
    
    style NodeJS fill:#00ff00,stroke:#00ff00,color:#000
    style LB fill:#ffb000,stroke:#ffb000,color:#000
```

---

**DIAGRAMS STATUS**: COMPREHENSIVE  
**VISUAL DOCUMENTATION**: COMPLETE  
**ARCHITECTURE**: ILLUSTRATED  

`[END OF TAPE]`

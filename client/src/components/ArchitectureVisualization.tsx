import React, { useState, useEffect } from 'react';

interface ArchitectureVisualizationProps {
  isVisible: boolean;
  onClose: () => void;
}

type AnimationPhase = 'idle' | 'ui-to-server' | 'server-to-legacy' | 'legacy-processing' | 'legacy-to-server' | 'server-to-ui';

export const ArchitectureVisualization: React.FC<ArchitectureVisualizationProps> = ({ isVisible, onClose }) => {
  const [animationPhase, setAnimationPhase] = useState<AnimationPhase>('idle');
  const [isAnimating, setIsAnimating] = useState(false);
  const [selectedLanguage, setSelectedLanguage] = useState<'cobol' | 'fortran' | 'pascal' | 'basic'>('cobol');

  // AUTO-PLAY ANIMATION ON MOUNT
  useEffect(() => {
    if (isVisible && !isAnimating) {
      startAnimation();
    }
  }, [isVisible]);

  const startAnimation = () => {
    setIsAnimating(true);
    setAnimationPhase('ui-to-server');

    setTimeout(() => setAnimationPhase('server-to-legacy'), 800);
    setTimeout(() => setAnimationPhase('legacy-processing'), 1600);
    setTimeout(() => setAnimationPhase('legacy-to-server'), 2400);
    setTimeout(() => setAnimationPhase('server-to-ui'), 3200);
    setTimeout(() => {
      setAnimationPhase('idle');
      setIsAnimating(false);
    }, 4000);
  };

  const exportDiagram = () => {
    const svg = document.getElementById('architecture-svg');
    if (!svg) return;

    const svgData = new XMLSerializer().serializeToString(svg);
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');
    const img = new Image();

    canvas.width = 1200;
    canvas.height = 800;

    img.onload = () => {
      ctx?.drawImage(img, 0, 0);
      const pngFile = canvas.toDataURL('image/png');
      
      const downloadLink = document.createElement('a');
      downloadLink.download = 'necro-bridge-architecture.png';
      downloadLink.href = pngFile;
      downloadLink.click();
    };

    img.src = 'data:image/svg+xml;base64,' + btoa(svgData);
  };

  if (!isVisible) return null;

  const languageInfo = {
    cobol: { year: '1959', color: '#00ff00', name: 'COBOL' },
    fortran: { year: '1957', color: '#ffb000', name: 'FORTRAN' },
    pascal: { year: '1970', color: '#00bfff', name: 'PASCAL' },
    basic: { year: '1983', color: '#ff69b4', name: 'BASIC' }
  };

  const currentLang = languageInfo[selectedLanguage];

  return (
    <div className="fixed inset-0 bg-black bg-opacity-95 z-50 flex items-center justify-center p-4">
      <div className="bg-mainframe-black border-4 border-mainframe-green w-full max-w-6xl max-h-[90vh] overflow-auto">
        {/* HEADER */}
        <div className="border-b-2 border-mainframe-green p-4 flex justify-between items-center">
          <h2 className="text-2xl font-mono text-mainframe-green">
            🏛️ ARCHITECTURE VISUALIZATION
          </h2>
          <div className="flex gap-4">
            <button
              onClick={exportDiagram}
              className="bg-black border-2 border-mainframe-green text-mainframe-green font-mono py-2 px-4 hover:bg-mainframe-green hover:text-black transition-colors text-sm"
            >
              💾 EXPORT PNG
            </button>
            <button
              onClick={startAnimation}
              disabled={isAnimating}
              className="bg-black border-2 border-mainframe-green text-mainframe-green font-mono py-2 px-4 hover:bg-mainframe-green hover:text-black transition-colors text-sm disabled:opacity-50"
            >
              ▶️ REPLAY
            </button>
            <button
              onClick={onClose}
              className="bg-black border-2 border-mainframe-green text-mainframe-green font-mono py-2 px-4 hover:bg-mainframe-green hover:text-black transition-colors text-sm"
            >
              ✕ CLOSE
            </button>
          </div>
        </div>

        {/* LANGUAGE SELECTOR */}
        <div className="border-b-2 border-mainframe-green p-4 flex gap-4 items-center">
          <span className="text-mainframe-green font-mono">SELECT LEGACY ENGINE:</span>
          {Object.entries(languageInfo).map(([key, info]) => (
            <button
              key={key}
              onClick={() => setSelectedLanguage(key as any)}
              className={`font-mono py-2 px-4 border-2 transition-colors ${
                selectedLanguage === key
                  ? 'bg-mainframe-green text-black border-mainframe-green'
                  : 'bg-black text-mainframe-green border-mainframe-green hover:bg-mainframe-green hover:text-black'
              }`}
            >
              {info.name} ({info.year})
            </button>
          ))}
        </div>

        {/* SVG DIAGRAM */}
        <div className="p-8">
          <svg
            id="architecture-svg"
            viewBox="0 0 1200 800"
            className="w-full h-auto"
            xmlns="http://www.w3.org/2000/svg"
          >
            {/* BACKGROUND */}
            <rect width="1200" height="800" fill="#000000" />

            {/* LAYER 1: NECRO-BANK UI */}
            <g id="ui-layer">
              <rect
                x="50"
                y="50"
                width="300"
                height="200"
                fill="none"
                stroke={animationPhase === 'ui-to-server' || animationPhase === 'server-to-ui' ? currentLang.color : '#00ff00'}
                strokeWidth="3"
                className={animationPhase === 'ui-to-server' || animationPhase === 'server-to-ui' ? 'animate-pulse' : ''}
              />
              <text x="200" y="80" textAnchor="middle" fill="#00ff00" fontSize="20" fontFamily="monospace" fontWeight="bold">
                NECRO-BANK UI
              </text>
              <text x="200" y="110" textAnchor="middle" fill="#00ff00" fontSize="14" fontFamily="monospace">
                React + TypeScript
              </text>
              
              {/* UI COMPONENTS */}
              <text x="70" y="140" fill="#00ff00" fontSize="12" fontFamily="monospace">• MortgageForm</text>
              <text x="70" y="160" fill="#00ff00" fontSize="12" fontFamily="monospace">• TerminalWindow</text>
              <text x="70" y="180" fill="#00ff00" fontSize="12" fontFamily="monospace">• PunchCard</text>
              <text x="70" y="200" fill="#00ff00" fontSize="12" fontFamily="monospace">• TapeReel</text>
              <text x="70" y="220" fill="#00ff00" fontSize="12" fontFamily="monospace">• PanelLights</text>
            </g>

            {/* LAYER 2: BRIDGE SERVER */}
            <g id="server-layer">
              <rect
                x="450"
                y="50"
                width="300"
                height="200"
                fill="none"
                stroke={animationPhase === 'server-to-legacy' || animationPhase === 'legacy-to-server' ? currentLang.color : '#00ff00'}
                strokeWidth="3"
                className={animationPhase === 'server-to-legacy' || animationPhase === 'legacy-to-server' ? 'animate-pulse' : ''}
              />
              <text x="600" y="80" textAnchor="middle" fill="#00ff00" fontSize="20" fontFamily="monospace" fontWeight="bold">
                BRIDGE SERVER
              </text>
              <text x="600" y="110" textAnchor="middle" fill="#00ff00" fontSize="14" fontFamily="monospace">
                Node.js + Express
              </text>
              
              {/* SERVER COMPONENTS */}
              <text x="470" y="140" fill="#00ff00" fontSize="12" fontFamily="monospace">• BridgeFactory</text>
              <text x="470" y="160" fill="#00ff00" fontSize="12" fontFamily="monospace">• CobolBridge</text>
              <text x="470" y="180" fill="#00ff00" fontSize="12" fontFamily="monospace">• FortranBridge</text>
              <text x="470" y="200" fill="#00ff00" fontSize="12" fontFamily="monospace">• PascalBridge</text>
              <text x="470" y="220" fill="#00ff00" fontSize="12" fontFamily="monospace">• BasicBridge</text>
            </g>

            {/* LAYER 3: LEGACY ENGINES */}
            <g id="legacy-layer">
              <rect
                x="850"
                y="50"
                width="300"
                height="200"
                fill="none"
                stroke={animationPhase === 'legacy-processing' ? currentLang.color : '#00ff00'}
                strokeWidth="3"
                className={animationPhase === 'legacy-processing' ? 'animate-pulse' : ''}
              />
              <text x="1000" y="80" textAnchor="middle" fill={currentLang.color} fontSize="20" fontFamily="monospace" fontWeight="bold">
                {currentLang.name} ENGINE
              </text>
              <text x="1000" y="110" textAnchor="middle" fill={currentLang.color} fontSize="14" fontFamily="monospace">
                Vintage {currentLang.year}
              </text>
              
              {/* LEGACY COMPONENTS */}
              <text x="870" y="140" fill={currentLang.color} fontSize="12" fontFamily="monospace">
                {selectedLanguage === 'cobol' && '• mortgage.cbl'}
                {selectedLanguage === 'fortran' && '• trajectory.f'}
                {selectedLanguage === 'pascal' && '• tax.pas'}
                {selectedLanguage === 'basic' && '• interest.bas'}
              </text>
              <text x="870" y="160" fill={currentLang.color} fontSize="12" fontFamily="monospace">• Argument Parser</text>
              <text x="870" y="180" fill={currentLang.color} fontSize="12" fontFamily="monospace">• Calculator Logic</text>
              <text x="870" y="200" fill={currentLang.color} fontSize="12" fontFamily="monospace">• STDOUT Writer</text>
              <text x="870" y="220" fill={currentLang.color} fontSize="12" fontFamily="monospace">• Error Handler</text>
            </g>

            {/* DATA FLOW ARROWS */}
            {/* UI TO SERVER */}
            <g id="arrow-ui-to-server">
              <defs>
                <marker id="arrowhead-right" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
                  <polygon points="0 0, 10 3, 0 6" fill={animationPhase === 'ui-to-server' ? currentLang.color : '#00ff00'} />
                </marker>
              </defs>
              <line
                x1="350"
                y1="150"
                x2="450"
                y2="150"
                stroke={animationPhase === 'ui-to-server' ? currentLang.color : '#00ff00'}
                strokeWidth="2"
                markerEnd="url(#arrowhead-right)"
                className={animationPhase === 'ui-to-server' ? 'animate-pulse' : ''}
              />
              <text x="400" y="140" textAnchor="middle" fill="#00ff00" fontSize="11" fontFamily="monospace">
                POST /api/calculate
              </text>
            </g>

            {/* SERVER TO LEGACY */}
            <g id="arrow-server-to-legacy">
              <defs>
                <marker id="arrowhead-right-2" markerWidth="10" markerHeight="10" refX="9" refY="3" orient="auto">
                  <polygon points="0 0, 10 3, 0 6" fill={animationPhase === 'server-to-legacy' ? currentLang.color : '#00ff00'} />
                </marker>
              </defs>
              <line
                x1="750"
                y1="150"
                x2="850"
                y2="150"
                stroke={animationPhase === 'server-to-legacy' ? currentLang.color : '#00ff00'}
                strokeWidth="2"
                markerEnd="url(#arrowhead-right-2)"
                className={animationPhase === 'server-to-legacy' ? 'animate-pulse' : ''}
              />
              <text x="800" y="140" textAnchor="middle" fill="#00ff00" fontSize="11" fontFamily="monospace">
                exec(binary)
              </text>
            </g>

            {/* LEGACY TO SERVER (RETURN) */}
            <g id="arrow-legacy-to-server">
              <defs>
                <marker id="arrowhead-left" markerWidth="10" markerHeight="10" refX="1" refY="3" orient="auto">
                  <polygon points="10 0, 0 3, 10 6" fill={animationPhase === 'legacy-to-server' ? currentLang.color : '#00ff00'} />
                </marker>
              </defs>
              <line
                x1="850"
                y1="180"
                x2="750"
                y2="180"
                stroke={animationPhase === 'legacy-to-server' ? currentLang.color : '#00ff00'}
                strokeWidth="2"
                markerEnd="url(#arrowhead-left)"
                className={animationPhase === 'legacy-to-server' ? 'animate-pulse' : ''}
              />
              <text x="800" y="200" textAnchor="middle" fill="#00ff00" fontSize="11" fontFamily="monospace">
                RESULT: XX.XX
              </text>
            </g>

            {/* SERVER TO UI (RETURN) */}
            <g id="arrow-server-to-ui">
              <defs>
                <marker id="arrowhead-left-2" markerWidth="10" markerHeight="10" refX="1" refY="3" orient="auto">
                  <polygon points="10 0, 0 3, 10 6" fill={animationPhase === 'server-to-ui' ? currentLang.color : '#00ff00'} />
                </marker>
              </defs>
              <line
                x1="450"
                y1="180"
                x2="350"
                y2="180"
                stroke={animationPhase === 'server-to-ui' ? currentLang.color : '#00ff00'}
                strokeWidth="2"
                markerEnd="url(#arrowhead-left-2)"
                className={animationPhase === 'server-to-ui' ? 'animate-pulse' : ''}
              />
              <text x="400" y="200" textAnchor="middle" fill="#00ff00" fontSize="11" fontFamily="monospace">
                JSON Response
              </text>
            </g>

            {/* KIRO INTEGRATION LAYER */}
            <g id="kiro-layer">
              <rect
                x="50"
                y="300"
                width="1100"
                height="150"
                fill="none"
                stroke="#ffb000"
                strokeWidth="3"
                strokeDasharray="10,5"
              />
              <text x="600" y="330" textAnchor="middle" fill="#ffb000" fontSize="20" fontFamily="monospace" fontWeight="bold">
                KIRO AUTOMATION LAYER
              </text>
              
              {/* KIRO COMPONENTS */}
              <g id="kiro-hooks">
                <rect x="100" y="350" width="250" height="80" fill="none" stroke="#ffb000" strokeWidth="2" />
                <text x="225" y="375" textAnchor="middle" fill="#ffb000" fontSize="14" fontFamily="monospace" fontWeight="bold">
                  AGENT HOOKS
                </text>
                <text x="120" y="395" fill="#ffb000" fontSize="11" fontFamily="monospace">• Auto-Compilation</text>
                <text x="120" y="410" fill="#ffb000" fontSize="11" fontFamily="monospace">• File Watch (*.cbl, *.f)</text>
                <text x="120" y="425" fill="#ffb000" fontSize="11" fontFamily="monospace">• Build Notifications</text>
              </g>

              <g id="kiro-mcp">
                <rect x="425" y="350" width="350" height="80" fill="none" stroke="#ffb000" strokeWidth="2" />
                <text x="600" y="375" textAnchor="middle" fill="#ffb000" fontSize="14" fontFamily="monospace" fontWeight="bold">
                  MCP TOOLS
                </text>
                <text x="445" y="395" fill="#ffb000" fontSize="11" fontFamily="monospace">• run_legacy_calc (Direct Testing)</text>
                <text x="445" y="410" fill="#ffb000" fontSize="11" fontFamily="monospace">• summon_ancient_spirit (AI Code Gen)</text>
                <text x="445" y="425" fill="#ffb000" fontSize="11" fontFamily="monospace">• GPT-4 Integration</text>
              </g>

              <g id="kiro-steering">
                <rect x="850" y="350" width="250" height="80" fill="none" stroke="#ffb000" strokeWidth="2" />
                <text x="975" y="375" textAnchor="middle" fill="#ffb000" fontSize="14" fontFamily="monospace" fontWeight="bold">
                  STEERING
                </text>
                <text x="870" y="395" fill="#ffb000" fontSize="11" fontFamily="monospace">• IBM 7090 Persona</text>
                <text x="870" y="410" fill="#ffb000" fontSize="11" fontFamily="monospace">• Vintage Terminology</text>
                <text x="870" y="425" fill="#ffb000" fontSize="11" fontFamily="monospace">• Context Injection</text>
              </g>
            </g>

            {/* TOOLKIT LAYER */}
            <g id="toolkit-layer">
              <rect
                x="50"
                y="500"
                width="1100"
                height="250"
                fill="none"
                stroke="#00bfff"
                strokeWidth="3"
              />
              <text x="600" y="530" textAnchor="middle" fill="#00bfff" fontSize="20" fontFamily="monospace" fontWeight="bold">
                LEGACY MODERNIZATION TOOLKIT (@necro-bridge/core)
              </text>
              
              {/* CLI COMMANDS */}
              <g id="cli-commands">
                <text x="100" y="560" fill="#00bfff" fontSize="14" fontFamily="monospace" fontWeight="bold">
                  CLI COMMANDS:
                </text>
                <text x="100" y="580" fill="#00bfff" fontSize="12" fontFamily="monospace">$ necro-bridge init &lt;language&gt;</text>
                <text x="100" y="600" fill="#00bfff" fontSize="12" fontFamily="monospace">$ necro-bridge detect</text>
                <text x="100" y="620" fill="#00bfff" fontSize="12" fontFamily="monospace">$ necro-bridge serve</text>
                <text x="100" y="640" fill="#00bfff" fontSize="12" fontFamily="monospace">$ necro-bridge test &lt;binary&gt;</text>
              </g>

              {/* TEMPLATES */}
              <g id="templates">
                <text x="500" y="560" fill="#00bfff" fontSize="14" fontFamily="monospace" fontWeight="bold">
                  BRIDGE TEMPLATES:
                </text>
                <text x="500" y="580" fill="#00bfff" fontSize="12" fontFamily="monospace">• cobol-bridge.js</text>
                <text x="500" y="600" fill="#00bfff" fontSize="12" fontFamily="monospace">• fortran-bridge.js</text>
                <text x="500" y="620" fill="#00bfff" fontSize="12" fontFamily="monospace">• pascal-bridge.js</text>
                <text x="500" y="640" fill="#00bfff" fontSize="12" fontFamily="monospace">• basic-bridge.js</text>
                <text x="500" y="660" fill="#00bfff" fontSize="12" fontFamily="monospace">• universal-server.js</text>
              </g>

              {/* FEATURES */}
              <g id="features">
                <text x="800" y="560" fill="#00bfff" fontSize="14" fontFamily="monospace" fontWeight="bold">
                  FEATURES:
                </text>
                <text x="800" y="580" fill="#00bfff" fontSize="12" fontFamily="monospace">• Auto-Detection</text>
                <text x="800" y="600" fill="#00bfff" fontSize="12" fontFamily="monospace">• JSON API Generation</text>
                <text x="800" y="620" fill="#00bfff" fontSize="12" fontFamily="monospace">• Error Handling</text>
                <text x="800" y="640" fill="#00bfff" fontSize="12" fontFamily="monospace">• Docker Support</text>
                <text x="800" y="660" fill="#00bfff" fontSize="12" fontFamily="monospace">• TypeScript Definitions</text>
              </g>

              {/* NPM BADGE */}
              <rect x="450" y="690" width="300" height="40" fill="#00bfff" />
              <text x="600" y="715" textAnchor="middle" fill="#000000" fontSize="16" fontFamily="monospace" fontWeight="bold">
                npm install @necro-bridge/core
              </text>
            </g>

            {/* LEGEND */}
            <g id="legend">
              <text x="50" y="780" fill="#00ff00" fontSize="12" fontFamily="monospace">
                ━━━ Active Layer  ┄┄┄ Integration Layer  ▶ Data Flow
              </text>
            </g>
          </svg>
        </div>

        {/* ANIMATION STATUS */}
        <div className="border-t-2 border-mainframe-green p-4">
          <div className="text-mainframe-green font-mono text-sm">
            <span className="font-bold">ANIMATION STATUS:</span>{' '}
            {animationPhase === 'idle' && 'IDLE - READY FOR CALCULATION'}
            {animationPhase === 'ui-to-server' && 'USER INPUT → BRIDGE SERVER'}
            {animationPhase === 'server-to-legacy' && `BRIDGE SERVER → ${currentLang.name} ENGINE`}
            {animationPhase === 'legacy-processing' && `${currentLang.name} ENGINE PROCESSING...`}
            {animationPhase === 'legacy-to-server' && `${currentLang.name} ENGINE → BRIDGE SERVER`}
            {animationPhase === 'server-to-ui' && 'BRIDGE SERVER → USER INTERFACE'}
          </div>
        </div>
      </div>
    </div>
  );
};

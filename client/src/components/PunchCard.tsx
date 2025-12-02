import React, { useState } from 'react';

// IBM 029 Keypunch encoding table
// Each character maps to an array of row numbers (12, 11, 0, 1-9)
// Row 12 is the top row, Row 9 is the bottom row
const IBM_029_ENCODING: Record<string, number[]> = {
  // Digits (single punch in rows 0-9)
  '0': [0],
  '1': [1],
  '2': [2],
  '3': [3],
  '4': [4],
  '5': [5],
  '6': [6],
  '7': [7],
  '8': [8],
  '9': [9],
  
  // Letters A-I (12 punch + digit punch)
  'A': [12, 1],
  'B': [12, 2],
  'C': [12, 3],
  'D': [12, 4],
  'E': [12, 5],
  'F': [12, 6],
  'G': [12, 7],
  'H': [12, 8],
  'I': [12, 9],
  
  // Letters J-R (11 punch + digit punch)
  'J': [11, 1],
  'K': [11, 2],
  'L': [11, 3],
  'M': [11, 4],
  'N': [11, 5],
  'O': [11, 6],
  'P': [11, 7],
  'Q': [11, 8],
  'R': [11, 9],
  
  // Letters S-Z (0 punch + digit punch)
  'S': [0, 2],
  'T': [0, 3],
  'U': [0, 4],
  'V': [0, 5],
  'W': [0, 6],
  'X': [0, 7],
  'Y': [0, 8],
  'Z': [0, 9],
  
  // Special characters
  ' ': [], // No punches for space
  '.': [12, 3, 8], // Period
  ',': [0, 3, 8], // Comma
  '(': [12, 5, 8], // Left parenthesis
  ')': [11, 5, 8], // Right parenthesis
  '+': [12, 6, 8], // Plus
  '-': [11], // Minus/hyphen
  '*': [11, 4, 8], // Asterisk
  '/': [0, 1], // Slash
  '=': [3, 8], // Equals
  '$': [11, 3, 8], // Dollar sign
  ':': [2, 8], // Colon
  ';': [11, 6, 8], // Semicolon
  '"': [7, 8], // Quote
  "'": [5, 8], // Apostrophe
};

// Row names for display
const ROW_NAMES = ['12', '11', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

interface PunchCardProps {
  text: string;
  maxColumns?: number;
}

const PunchCard: React.FC<PunchCardProps> = ({ text, maxColumns = 80 }) => {
  const [hoveredColumn, setHoveredColumn] = useState<number | null>(null);
  
  // Pad or truncate text to maxColumns
  const paddedText = text.toUpperCase().padEnd(maxColumns, ' ').slice(0, maxColumns);
  
  // Determine if a hole should be punched at given column and row
  const isPunched = (column: number, row: number): boolean => {
    const char = paddedText[column];
    const encoding = IBM_029_ENCODING[char] || [];
    return encoding.includes(row);
  };
  
  // Determine if a punch is a control punch (zone rows: 12, 11, 0)
  const isControlPunch = (row: number): boolean => {
    return row === 12 || row === 11 || row === 0;
  };
  
  return (
    <div className="punch-card-container bg-[#f5e6d3] p-4 rounded-lg shadow-lg border-2 border-mainframe-green">
      <div className="mb-2 text-xs font-mono text-gray-700 text-center">
        IBM 029 KEYPUNCH CARD - 80 COLUMNS × 12 ROWS
      </div>
      
      {/* Card grid */}
      <div className="relative overflow-x-auto">
        <div className="inline-block min-w-full">
          {/* Row labels */}
          <div className="flex">
            <div className="w-8 flex-shrink-0"></div>
            {ROW_NAMES.map((rowName, idx) => (
              <div
                key={`row-label-${idx}`}
                className="text-[6px] font-mono text-gray-600 text-center mb-1"
                style={{ width: '8px' }}
              >
                {rowName}
              </div>
            ))}
          </div>
          
          {/* Card columns */}
          {Array.from({ length: maxColumns }).map((_, colIdx) => (
            <div
              key={`col-${colIdx}`}
              className="flex items-center hover:bg-yellow-100 transition-colors relative"
              onMouseEnter={() => setHoveredColumn(colIdx)}
              onMouseLeave={() => setHoveredColumn(null)}
            >
              {/* Column number */}
              <div className="w-8 flex-shrink-0 text-[6px] font-mono text-gray-600 text-right pr-1">
                {colIdx + 1}
              </div>
              
              {/* Punch holes for this column */}
              {ROW_NAMES.map((_, rowIdx) => {
                const row = rowIdx === 0 ? 12 : rowIdx === 1 ? 11 : rowIdx === 2 ? 0 : rowIdx - 2;
                const punched = isPunched(colIdx, row);
                const isControl = isControlPunch(row);
                
                return (
                  <div
                    key={`hole-${colIdx}-${rowIdx}`}
                    className="relative"
                    style={{ width: '8px', height: '8px' }}
                  >
                    {punched ? (
                      <div
                        className={`w-2 h-3 rounded-sm mx-auto ${
                          isControl ? 'bg-red-600' : 'bg-gray-800'
                        }`}
                        title={`Row ${row}`}
                      />
                    ) : (
                      <div className="w-1 h-1 rounded-full bg-gray-300 mx-auto" />
                    )}
                  </div>
                );
              })}
              
              {/* Tooltip showing character */}
              {hoveredColumn === colIdx && (
                <div className="absolute left-0 top-full mt-1 bg-black text-mainframe-green px-2 py-1 rounded text-xs font-mono whitespace-nowrap z-10 shadow-lg">
                  <div>Col {colIdx + 1}: '{paddedText[colIdx]}'</div>
                  <div className="text-[10px] text-gray-400">
                    Rows: {IBM_029_ENCODING[paddedText[colIdx]]?.join(', ') || 'none'}
                  </div>
                </div>
              )}
            </div>
          ))}
        </div>
      </div>
      
      {/* Legend */}
      <div className="mt-3 flex gap-4 justify-center text-xs font-mono">
        <div className="flex items-center gap-1">
          <div className="w-3 h-3 bg-gray-800 rounded-sm"></div>
          <span className="text-gray-700">Data Holes</span>
        </div>
        <div className="flex items-center gap-1">
          <div className="w-3 h-3 bg-red-600 rounded-sm"></div>
          <span className="text-gray-700">Control Holes (Zone)</span>
        </div>
      </div>
    </div>
  );
};

export default PunchCard;

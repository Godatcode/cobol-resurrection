import React, { useEffect, useState } from 'react';

interface MemoryDumpProps {
  errorData?: {
    error: string;
    details?: string;
  } | null;
  isVisible: boolean;
}

/**
 * MemoryDump Component - Core Memory Hexadecimal Dump Visualization
 * 
 * Features:
 * - Hexadecimal memory display in 16-byte rows
 * - Color coding: code (green), data (yellow), stack (red)
 * - ASCII representation column
 * - Triggered on error responses from bridge server
 * 
 * Requirements: 11.4
 */
function MemoryDump({ errorData, isVisible }: MemoryDumpProps) {
  const [memoryData, setMemoryData] = useState<MemoryRow[]>([]);

  interface MemoryRow {
    address: string;
    bytes: MemoryByte[];
    ascii: string;
  }

  interface MemoryByte {
    value: string;
    type: 'code' | 'data' | 'stack';
  }

  // Generate pseudo-random but deterministic memory dump based on error
  useEffect(() => {
    if (!errorData || !isVisible) {
      return;
    }

    // Generate memory dump based on error details
    const rows: MemoryRow[] = [];
    const baseAddress = 0x1000; // Start at typical code segment address
    const numRows = 16; // Display 16 rows (256 bytes total)

    // Create seed from error message for deterministic output
    const seed = errorData.error.split('').reduce((acc, char) => acc + char.charCodeAt(0), 0);

    for (let i = 0; i < numRows; i++) {
      const address = (baseAddress + i * 16).toString(16).toUpperCase().padStart(8, '0');
      const bytes: MemoryByte[] = [];
      let ascii = '';

      for (let j = 0; j < 16; j++) {
        // Generate pseudo-random byte value
        const byteValue = ((seed * (i * 16 + j) * 1103515245 + 12345) & 0xFF);
        const hexValue = byteValue.toString(16).toUpperCase().padStart(2, '0');

        // Determine memory type based on address region
        let type: 'code' | 'data' | 'stack';
        if (i < 6) {
          // First 6 rows are code segment
          type = 'code';
        } else if (i < 12) {
          // Next 6 rows are data segment
          type = 'data';
        } else {
          // Last 4 rows are stack segment
          type = 'stack';
        }

        bytes.push({ value: hexValue, type });

        // Generate ASCII representation
        // Only show printable ASCII characters (32-126)
        if (byteValue >= 32 && byteValue <= 126) {
          ascii += String.fromCharCode(byteValue);
        } else {
          ascii += '.';
        }
      }

      rows.push({ address, bytes, ascii });
    }

    setMemoryData(rows);
  }, [errorData, isVisible]);

  if (!isVisible || !errorData) {
    return null;
  }

  // Get color class based on memory type
  const getColorClass = (type: 'code' | 'data' | 'stack'): string => {
    switch (type) {
      case 'code':
        return 'text-mainframe-green';
      case 'data':
        return 'text-yellow-400';
      case 'stack':
        return 'text-red-500';
      default:
        return 'text-mainframe-green';
    }
  };

  return (
    <div className="memory-dump-container bg-[#1a1a1a] p-6 rounded-lg border-2 border-red-500 shadow-lg max-w-6xl mx-auto my-6">
      {/* Header */}
      <div className="text-center mb-4 border-b-2 border-red-500 pb-3">
        <div className="font-mono text-red-500 text-lg font-bold animate-pulse">
          ⚠️ CORE DUMP DETECTED ⚠️
        </div>
        <div className="font-mono text-mainframe-green text-xs mt-2">
          SYSTEM FAILURE: {errorData.error}
        </div>
        {errorData.details && (
          <div className="font-mono text-gray-400 text-xs mt-1">
            {errorData.details}
          </div>
        )}
      </div>

      {/* Memory Legend */}
      <div className="flex justify-center gap-6 mb-4 text-xs font-mono">
        <div className="flex items-center gap-2">
          <div className="w-3 h-3 bg-mainframe-green rounded"></div>
          <span className="text-mainframe-green">CODE SEGMENT</span>
        </div>
        <div className="flex items-center gap-2">
          <div className="w-3 h-3 bg-yellow-400 rounded"></div>
          <span className="text-yellow-400">DATA SEGMENT</span>
        </div>
        <div className="flex items-center gap-2">
          <div className="w-3 h-3 bg-red-500 rounded"></div>
          <span className="text-red-500">STACK SEGMENT</span>
        </div>
      </div>

      {/* Memory Dump Display */}
      <div className="bg-black p-4 rounded border border-gray-700 overflow-x-auto">
        <div className="font-mono text-xs">
          {/* Column Headers */}
          <div className="flex gap-2 mb-2 pb-2 border-b border-gray-700">
            <div className="text-gray-500 w-20">ADDRESS</div>
            <div className="text-gray-500 flex-1">
              HEXADECIMAL DUMP (16 BYTES)
            </div>
            <div className="text-gray-500 w-20">ASCII</div>
          </div>

          {/* Memory Rows */}
          {memoryData.map((row, rowIdx) => (
            <div key={`row-${rowIdx}`} className="flex gap-2 mb-1 hover:bg-gray-900 transition-colors">
              {/* Address Column */}
              <div className="text-gray-400 w-20 font-bold">
                {row.address}
              </div>

              {/* Hexadecimal Bytes */}
              <div className="flex-1 flex gap-1">
                {row.bytes.map((byte, byteIdx) => (
                  <span
                    key={`byte-${rowIdx}-${byteIdx}`}
                    className={`${getColorClass(byte.type)} ${byteIdx === 7 ? 'mr-2' : ''}`}
                  >
                    {byte.value}
                  </span>
                ))}
              </div>

              {/* ASCII Representation */}
              <div className="text-gray-500 w-20 font-mono">
                {row.ascii}
              </div>
            </div>
          ))}
        </div>
      </div>

      {/* Footer */}
      <div className="mt-4 text-center">
        <div className="font-mono text-gray-500 text-xs">
          MEMORY SNAPSHOT AT TIME OF FAILURE
        </div>
        <div className="font-mono text-red-500 text-xs mt-1">
          SYSTEM HALTED - OPERATOR INTERVENTION REQUIRED
        </div>
      </div>
    </div>
  );
}

export default MemoryDump;

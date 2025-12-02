/**
 * CODE GENERATOR MODAL TESTS
 * Tests for the AI-powered legacy code generation interface
 * Validates Requirements 10.1, 10.2, 13.2
 */

import React from 'react';
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import axios from 'axios';
import CodeGeneratorModal from '../client/src/components/CodeGeneratorModal';

// Mock axios
vi.mock('axios');

describe('CodeGeneratorModal', () => {
  const mockOnClose = vi.fn();
  const mockOnGenerationComplete = vi.fn();

  beforeEach(() => {
    vi.clearAllMocks();
  });

  it('should not render when isOpen is false', () => {
    const { container } = render(
      <CodeGeneratorModal
        isOpen={false}
        onClose={mockOnClose}
        onGenerationComplete={mockOnGenerationComplete}
      />
    );
    
    expect(container.firstChild).toBeNull();
  });

  it('should render modal when isOpen is true', () => {
    render(
      <CodeGeneratorModal
        isOpen={true}
        onClose={mockOnClose}
        onGenerationComplete={mockOnGenerationComplete}
      />
    );
    
    expect(screen.getByText(/SUMMON ANCIENT SPIRIT/i)).toBeInTheDocument();
  });

  it('should display all four language options', () => {
    render(
      <CodeGeneratorModal
        isOpen={true}
        onClose={mockOnClose}
        onGenerationComplete={mockOnGenerationComplete}
      />
    );
    
    expect(screen.getByText(/COBOL \(1959\)/i)).toBeInTheDocument();
    expect(screen.getByText(/FORTRAN \(1957\)/i)).toBeInTheDocument();
    expect(screen.getByText(/PASCAL \(1970\)/i)).toBeInTheDocument();
    expect(screen.getByText(/BASIC \(1983\)/i)).toBeInTheDocument();
  });

  it('should allow language selection', () => {
    render(
      <CodeGeneratorModal
        isOpen={true}
        onClose={mockOnClose}
        onGenerationComplete={mockOnGenerationComplete}
      />
    );
    
    const fortranButton = screen.getByText(/FORTRAN \(1957\)/i).closest('button');
    fireEvent.click(fortranButton);
    
    // FORTRAN button should have selected styling
    expect(fortranButton).toHaveClass('bg-mainframe-green');
  });

  it('should validate required fields before generation', async () => {
    render(
      <CodeGeneratorModal
        isOpen={true}
        onClose={mockOnClose}
        onGenerationComplete={mockOnGenerationComplete}
      />
    );
    
    const generateButton = screen.getByText(/GENERATE CODE/i);
    fireEvent.click(generateButton);
    
    await waitFor(() => {
      expect(screen.getByText(/DESCRIPTION REQUIRED/i)).toBeInTheDocument();
    });
  });

  it('should generate mock code when inputs are valid', async () => {
    render(
      <CodeGeneratorModal
        isOpen={true}
        onClose={mockOnClose}
        onGenerationComplete={mockOnGenerationComplete}
      />
    );
    
    // Fill in description
    const descriptionInput = screen.getByPlaceholderText(/Calculate compound interest/i);
    fireEvent.change(descriptionInput, { target: { value: 'Calculate simple interest' } });
    
    // Fill in filename
    const filenameInput = screen.getByPlaceholderText(/compound-interest/i);
    fireEvent.change(filenameInput, { target: { value: 'simple-interest' } });
    
    // Click generate
    const generateButton = screen.getByText(/GENERATE CODE/i);
    fireEvent.click(generateButton);
    
    // Should show generating message
    await waitFor(() => {
      expect(screen.getByText(/CHANNELING ANCIENT SPIRITS/i)).toBeInTheDocument();
    });
    
    // Should show generated code after delay
    await waitFor(() => {
      expect(screen.getByText(/GENERATED COBOL CODE/i)).toBeInTheDocument();
    }, { timeout: 3000 });
  });

  it('should display generated code with syntax highlighting', async () => {
    render(
      <CodeGeneratorModal
        isOpen={true}
        onClose={mockOnClose}
        onGenerationComplete={mockOnGenerationComplete}
      />
    );
    
    // Fill in inputs
    const descriptionInput = screen.getByPlaceholderText(/Calculate compound interest/i);
    fireEvent.change(descriptionInput, { target: { value: 'Test calculation' } });
    
    const filenameInput = screen.getByPlaceholderText(/compound-interest/i);
    fireEvent.change(filenameInput, { target: { value: 'test' } });
    
    // Generate code
    const generateButton = screen.getByText(/GENERATE CODE/i);
    fireEvent.click(generateButton);
    
    // Wait for code to be displayed
    await waitFor(() => {
      const codeBlock = screen.getByText(/IDENTIFICATION DIVISION/i);
      expect(codeBlock).toBeInTheDocument();
    }, { timeout: 3000 });
  });

  it('should call API when compile button is clicked', async () => {
    axios.post.mockResolvedValue({
      data: {
        success: true,
        message: 'Code compiled successfully',
        file_path: '/legacy/cobol/test.cbl',
        filename: 'test',
        compilation: {
          success: true,
          compiler: 'cobc',
          message: 'Compilation successful'
        }
      }
    });

    render(
      <CodeGeneratorModal
        isOpen={true}
        onClose={mockOnClose}
        onGenerationComplete={mockOnGenerationComplete}
      />
    );
    
    // Fill in inputs and generate
    const descriptionInput = screen.getByPlaceholderText(/Calculate compound interest/i);
    fireEvent.change(descriptionInput, { target: { value: 'Test calculation' } });
    
    const filenameInput = screen.getByPlaceholderText(/compound-interest/i);
    fireEvent.change(filenameInput, { target: { value: 'test' } });
    
    const generateButton = screen.getByText(/GENERATE CODE/i);
    fireEvent.click(generateButton);
    
    // Wait for code generation
    await waitFor(() => {
      expect(screen.getByText(/COMPILE & SAVE/i)).toBeInTheDocument();
    }, { timeout: 3000 });
    
    // Click compile
    const compileButton = screen.getByText(/COMPILE & SAVE/i);
    fireEvent.click(compileButton);
    
    // Verify API was called
    await waitFor(() => {
      expect(axios.post).toHaveBeenCalledWith(
        'http://localhost:3001/api/generate',
        expect.objectContaining({
          language: 'COBOL',
          filename: 'test'
        })
      );
    });
  });

  it('should show compilation status in real-time', async () => {
    axios.post.mockResolvedValue({
      data: {
        success: true,
        message: 'Code compiled successfully',
        file_path: '/legacy/cobol/test.cbl',
        filename: 'test',
        compilation: {
          success: true,
          compiler: 'cobc',
          message: '👻 Ancient Spirit Rebound to Binary: test'
        }
      }
    });

    render(
      <CodeGeneratorModal
        isOpen={true}
        onClose={mockOnClose}
        onGenerationComplete={mockOnGenerationComplete}
      />
    );
    
    // Fill in inputs and generate
    const descriptionInput = screen.getByPlaceholderText(/Calculate compound interest/i);
    fireEvent.change(descriptionInput, { target: { value: 'Test calculation' } });
    
    const filenameInput = screen.getByPlaceholderText(/compound-interest/i);
    fireEvent.change(filenameInput, { target: { value: 'test' } });
    
    const generateButton = screen.getByText(/GENERATE CODE/i);
    fireEvent.click(generateButton);
    
    // Wait for code generation
    await waitFor(() => {
      expect(screen.getByText(/COMPILE & SAVE/i)).toBeInTheDocument();
    }, { timeout: 3000 });
    
    // Click compile
    const compileButton = screen.getByText(/COMPILE & SAVE/i);
    fireEvent.click(compileButton);
    
    // Should show compiling status
    await waitFor(() => {
      expect(screen.getByText(/BINDING SPIRIT TO BINARY/i)).toBeInTheDocument();
    });
    
    // Should show success message
    await waitFor(() => {
      expect(screen.getByText(/ANCIENT SPIRIT SUMMONED/i)).toBeInTheDocument();
      expect(screen.getByText(/Ancient Spirit Rebound to Binary/i)).toBeInTheDocument();
    });
  });

  it('should handle compilation errors gracefully', async () => {
    axios.post.mockRejectedValue({
      response: {
        data: {
          error: 'SYNTAX VALIDATION FAILED',
          details: ['Missing IDENTIFICATION DIVISION']
        }
      }
    });

    render(
      <CodeGeneratorModal
        isOpen={true}
        onClose={mockOnClose}
        onGenerationComplete={mockOnGenerationComplete}
      />
    );
    
    // Fill in inputs and generate
    const descriptionInput = screen.getByPlaceholderText(/Calculate compound interest/i);
    fireEvent.change(descriptionInput, { target: { value: 'Test calculation' } });
    
    const filenameInput = screen.getByPlaceholderText(/compound-interest/i);
    fireEvent.change(filenameInput, { target: { value: 'test' } });
    
    const generateButton = screen.getByText(/GENERATE CODE/i);
    fireEvent.click(generateButton);
    
    // Wait for code generation
    await waitFor(() => {
      expect(screen.getByText(/COMPILE & SAVE/i)).toBeInTheDocument();
    }, { timeout: 3000 });
    
    // Click compile
    const compileButton = screen.getByText(/COMPILE & SAVE/i);
    fireEvent.click(compileButton);
    
    // Should show error message
    await waitFor(() => {
      expect(screen.getByText(/COMPILATION ERROR/i)).toBeInTheDocument();
    });
  });

  it('should reset state when modal is closed', () => {
    const { rerender } = render(
      <CodeGeneratorModal
        isOpen={true}
        onClose={mockOnClose}
        onGenerationComplete={mockOnGenerationComplete}
      />
    );
    
    // Fill in some data
    const descriptionInput = screen.getByPlaceholderText(/Calculate compound interest/i);
    fireEvent.change(descriptionInput, { target: { value: 'Test calculation' } });
    
    // Close modal
    const closeButton = screen.getByText('[X]');
    fireEvent.click(closeButton);
    
    expect(mockOnClose).toHaveBeenCalled();
    
    // Reopen modal
    rerender(
      <CodeGeneratorModal
        isOpen={true}
        onClose={mockOnClose}
        onGenerationComplete={mockOnGenerationComplete}
      />
    );
    
    // Description should be empty
    const newDescriptionInput = screen.getByPlaceholderText(/Calculate compound interest/i);
    expect(newDescriptionInput.value).toBe('');
  });

  it('should call onGenerationComplete callback on successful compilation', async () => {
    const mockResponse = {
      data: {
        success: true,
        message: 'Code compiled successfully',
        file_path: '/legacy/cobol/test.cbl',
        filename: 'test',
        compilation: {
          success: true,
          compiler: 'cobc',
          message: 'Compilation successful'
        }
      }
    };

    axios.post.mockResolvedValue(mockResponse);

    render(
      <CodeGeneratorModal
        isOpen={true}
        onClose={mockOnClose}
        onGenerationComplete={mockOnGenerationComplete}
      />
    );
    
    // Fill in inputs and generate
    const descriptionInput = screen.getByPlaceholderText(/Calculate compound interest/i);
    fireEvent.change(descriptionInput, { target: { value: 'Test calculation' } });
    
    const filenameInput = screen.getByPlaceholderText(/compound-interest/i);
    fireEvent.change(filenameInput, { target: { value: 'test' } });
    
    const generateButton = screen.getByText(/GENERATE CODE/i);
    fireEvent.click(generateButton);
    
    // Wait for code generation
    await waitFor(() => {
      expect(screen.getByText(/COMPILE & SAVE/i)).toBeInTheDocument();
    }, { timeout: 3000 });
    
    // Click compile
    const compileButton = screen.getByText(/COMPILE & SAVE/i);
    fireEvent.click(compileButton);
    
    // Verify callback was called
    await waitFor(() => {
      expect(mockOnGenerationComplete).toHaveBeenCalledWith(mockResponse.data);
    });
  });
});

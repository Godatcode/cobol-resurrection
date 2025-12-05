import React from 'react';
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { render, screen, waitFor, fireEvent } from '@testing-library/react';
import BootSequence from './BootSequence';

describe('BootSequence Component', () => {
  let localStorageMock;

  beforeEach(() => {
    // Mock localStorage
    localStorageMock = {
      getItem: vi.fn(),
      setItem: vi.fn(),
      clear: vi.fn(),
    };
    global.localStorage = localStorageMock;
    
    // Clear all timers
    vi.clearAllTimers();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it('should render boot sequence on first load', () => {
    localStorageMock.getItem.mockReturnValue(null);
    const onComplete = vi.fn();

    render(<BootSequence onComplete={onComplete} />);

    // Check for title
    expect(screen.getByText(/NECRO-BRIDGE v1.0/i)).toBeInTheDocument();
    expect(screen.getByText(/LEGACY SYSTEM RESURRECTION PROTOCOL/i)).toBeInTheDocument();
  });

  it('should skip boot sequence if seen before', () => {
    localStorageMock.getItem.mockReturnValue('true');
    const onComplete = vi.fn();

    render(<BootSequence onComplete={onComplete} />);

    // Should call onComplete immediately
    expect(onComplete).toHaveBeenCalled();
  });

  it('should display boot messages sequentially', async () => {
    vi.useFakeTimers();
    localStorageMock.getItem.mockReturnValue(null);
    const onComplete = vi.fn();

    render(<BootSequence onComplete={onComplete} />);

    // First message should appear immediately
    expect(screen.getByText(/> NECRO-BRIDGE SYSTEM INITIALIZATION.../i)).toBeInTheDocument();

    // Advance time to show second message
    vi.advanceTimersByTime(600);
    await waitFor(() => {
      expect(screen.getByText(/> CHECKING CORE MEMORY... OK/i)).toBeInTheDocument();
    });

    vi.useRealTimers();
  });

  it('should update progress bar as boot progresses', async () => {
    vi.useFakeTimers();
    localStorageMock.getItem.mockReturnValue(null);
    const onComplete = vi.fn();

    render(<BootSequence onComplete={onComplete} />);

    // Initial progress should be low
    expect(screen.getByText(/0%/i)).toBeInTheDocument();

    // Advance time
    vi.advanceTimersByTime(3000);
    await waitFor(() => {
      // Progress should have increased
      const progressText = screen.getByText(/\d+%/);
      expect(progressText).toBeInTheDocument();
    });

    vi.useRealTimers();
  });

  it('should allow skipping boot sequence', async () => {
    localStorageMock.getItem.mockReturnValue(null);
    const onComplete = vi.fn();

    render(<BootSequence onComplete={onComplete} />);

    // Find and click skip button
    const skipButton = screen.getByText(/PRESS ANY KEY TO SKIP/i);
    fireEvent.click(skipButton);

    // Should set localStorage and call onComplete
    await waitFor(() => {
      expect(localStorageMock.setItem).toHaveBeenCalledWith('necro-bridge-boot-seen', 'true');
      expect(onComplete).toHaveBeenCalled();
    });
  });

  it('should mark boot as seen after completion', async () => {
    vi.useFakeTimers();
    localStorageMock.getItem.mockReturnValue(null);
    const onComplete = vi.fn();

    render(<BootSequence onComplete={onComplete} />);

    // Fast-forward through entire boot sequence (6000ms + 1000ms buffer)
    vi.advanceTimersByTime(7000);

    await waitFor(() => {
      expect(localStorageMock.setItem).toHaveBeenCalledWith('necro-bridge-boot-seen', 'true');
      expect(onComplete).toHaveBeenCalled();
    });

    vi.useRealTimers();
  });

  it('should display all boot messages in correct order', async () => {
    vi.useFakeTimers();
    localStorageMock.getItem.mockReturnValue(null);
    const onComplete = vi.fn();

    render(<BootSequence onComplete={onComplete} />);

    const expectedMessages = [
      'NECRO-BRIDGE SYSTEM INITIALIZATION...',
      'CHECKING CORE MEMORY... OK',
      'LOADING CHANNEL CONTROLLERS... OK',
      'INITIALIZING TAPE DRIVES... OK',
      'MOUNTING SYSTEM VOLUMES... OK',
      'LOADING COBOL RUNTIME... OK',
      'LOADING FORTRAN RUNTIME... OK',
      'LOADING PASCAL RUNTIME... OK',
      'LOADING BASIC RUNTIME... OK',
      'INITIALIZING NECRO-BRIDGE v1.0... OK',
      'SYSTEM READY',
    ];

    // Advance through all messages
    for (let i = 0; i < expectedMessages.length; i++) {
      vi.advanceTimersByTime(600);
      await waitFor(() => {
        expect(screen.getByText(new RegExp(`> ${expectedMessages[i]}`, 'i'))).toBeInTheDocument();
      });
    }

    vi.useRealTimers();
  });

  it('should render with authentic mainframe styling', () => {
    localStorageMock.getItem.mockReturnValue(null);
    const onComplete = vi.fn();

    const { container } = render(<BootSequence onComplete={onComplete} />);

    // Check for mainframe styling classes
    expect(container.querySelector('.bg-mainframe-black')).toBeInTheDocument();
    expect(container.querySelector('.text-mainframe-green')).toBeInTheDocument();
    expect(container.querySelector('.font-mono')).toBeInTheDocument();
  });
});

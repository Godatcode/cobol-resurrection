/**
 * DEMO MODE TESTS
 * Validates guided tour, tooltips, auto-play, and shareable demo link functionality
 * Requirements: 13.2, 13.3
 */

import React from 'react';
import { describe, it, expect, beforeEach, vi } from 'vitest';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import DemoMode from '../client/src/components/DemoMode';

describe('Demo Mode Component', () => {
  beforeEach(() => {
    // MOCK GETELEMENTBYID FOR TOOLTIP POSITIONING
    document.getElementById = vi.fn((id) => {
      return {
        getBoundingClientRect: () => ({
          top: 100,
          left: 100,
          bottom: 200,
          right: 200,
          width: 100,
          height: 100
        }),
        scrollIntoView: vi.fn(),
        classList: {
          add: vi.fn(),
          remove: vi.fn()
        }
      };
    });

    // MOCK QUERYSELECTORALL
    document.querySelectorAll = vi.fn(() => []);
  });

  it('should not render when isActive is false', () => {
    const { container } = render(<DemoMode isActive={false} />);
    expect(container.firstChild).toBeNull();
  });

  it('should render guided tour control panel when active', () => {
    render(<DemoMode isActive={true} />);
    
    expect(screen.getByText('GUIDED TOUR')).toBeDefined();
    expect(screen.getByText(/STEP 1 OF/)).toBeDefined();
  });

  it('should display current step tooltip with title and description', () => {
    render(<DemoMode isActive={true} />);
    
    // FIRST STEP SHOULD BE HEADER
    expect(screen.getByText('NECRO-BANK MAINFRAME SYSTEM')).toBeDefined();
    expect(screen.getByText(/WELCOME TO THE CYBER-NECROMANCY/)).toBeDefined();
  });

  it('should navigate to next step when NEXT button clicked', async () => {
    render(<DemoMode isActive={true} />);
    
    const nextButton = screen.getByText('NEXT →');
    fireEvent.click(nextButton);
    
    await waitFor(() => {
      expect(screen.getByText('STEP 2 OF')).toBeDefined();
      expect(screen.getByText('AI CODE GENERATOR')).toBeDefined();
    });
  });

  it('should navigate to previous step when PREVIOUS button clicked', async () => {
    render(<DemoMode isActive={true} />);
    
    // GO TO STEP 2
    const nextButton = screen.getByText('NEXT →');
    fireEvent.click(nextButton);
    
    await waitFor(() => {
      expect(screen.getByText('STEP 2 OF')).toBeDefined();
    });
    
    // GO BACK TO STEP 1
    const prevButton = screen.getByText('← PREVIOUS');
    fireEvent.click(prevButton);
    
    await waitFor(() => {
      expect(screen.getByText('STEP 1 OF')).toBeDefined();
    });
  });

  it('should disable PREVIOUS button on first step', () => {
    render(<DemoMode isActive={true} />);
    
    const prevButton = screen.getByText('← PREVIOUS');
    expect(prevButton.disabled).toBe(true);
  });

  it('should show FINISH button on last step', async () => {
    render(<DemoMode isActive={true} />);
    
    // NAVIGATE TO LAST STEP (9 STEPS TOTAL)
    const nextButton = screen.getByText('NEXT →');
    for (let i = 0; i < 8; i++) {
      fireEvent.click(nextButton);
      await waitFor(() => {
        expect(screen.getByText(`STEP ${i + 2} OF`)).toBeDefined();
      });
    }
    
    expect(screen.getByText('FINISH')).toBeDefined();
  });

  it('should call onClose when close button clicked', () => {
    const onClose = vi.fn();
    render(<DemoMode isActive={true} onClose={onClose} />);
    
    const closeButton = screen.getByText('✕');
    fireEvent.click(closeButton);
    
    expect(onClose).toHaveBeenCalled();
  });

  it('should start auto-play when AUTO-PLAY button clicked', async () => {
    render(<DemoMode isActive={true} />);
    
    const autoPlayButton = screen.getByText('▶ AUTO-PLAY');
    fireEvent.click(autoPlayButton);
    
    await waitFor(() => {
      expect(screen.getByText('⏸ PAUSE')).toBeDefined();
    });
  });

  it('should stop auto-play when PAUSE button clicked', async () => {
    render(<DemoMode isActive={true} />);
    
    // START AUTO-PLAY
    const autoPlayButton = screen.getByText('▶ AUTO-PLAY');
    fireEvent.click(autoPlayButton);
    
    await waitFor(() => {
      expect(screen.getByText('⏸ PAUSE')).toBeDefined();
    });
    
    // STOP AUTO-PLAY
    const pauseButton = screen.getByText('⏸ PAUSE');
    fireEvent.click(pauseButton);
    
    await waitFor(() => {
      expect(screen.getByText('▶ AUTO-PLAY')).toBeDefined();
    });
  });

  it('should open share modal when SHARE button clicked', async () => {
    render(<DemoMode isActive={true} />);
    
    const shareButton = screen.getByText('🔗 SHARE');
    fireEvent.click(shareButton);
    
    await waitFor(() => {
      expect(screen.getByText('SHARE DEMO LINK')).toBeDefined();
      expect(screen.getByText(/COPY THIS LINK/)).toBeDefined();
    });
  });

  it('should display shareable demo URL in share modal', async () => {
    render(<DemoMode isActive={true} />);
    
    const shareButton = screen.getByText('🔗 SHARE');
    fireEvent.click(shareButton);
    
    await waitFor(() => {
      const input = screen.getByDisplayValue(/\?demo=true/);
      expect(input).toBeDefined();
    });
  });

  it('should copy demo link to clipboard when COPY button clicked', async () => {
    // MOCK CLIPBOARD API
    const writeText = vi.fn();
    Object.assign(navigator, {
      clipboard: {
        writeText
      }
    });
    
    render(<DemoMode isActive={true} />);
    
    const shareButton = screen.getByText('🔗 SHARE');
    fireEvent.click(shareButton);
    
    await waitFor(() => {
      const copyButton = screen.getByText('COPY');
      fireEvent.click(copyButton);
    });
    
    expect(writeText).toHaveBeenCalledWith(expect.stringContaining('?demo=true'));
  });

  it('should show success message after copying link', async () => {
    // MOCK CLIPBOARD API
    Object.assign(navigator, {
      clipboard: {
        writeText: vi.fn().mockResolvedValue()
      }
    });
    
    render(<DemoMode isActive={true} />);
    
    const shareButton = screen.getByText('🔗 SHARE');
    fireEvent.click(shareButton);
    
    await waitFor(() => {
      const copyButton = screen.getByText('COPY');
      fireEvent.click(copyButton);
    });
    
    await waitFor(() => {
      expect(screen.getByText('✓ COPIED')).toBeDefined();
    });
  });

  it('should display progress bar reflecting current step', () => {
    const { container } = render(<DemoMode isActive={true} />);
    
    // STEP 1 OF 9 = 11.11% PROGRESS
    const progressBar = container.querySelector('.bg-mainframe-green');
    expect(progressBar.style.width).toBe('11.11111111111111%');
  });

  it('should display "Try it yourself" prompt', () => {
    render(<DemoMode isActive={true} />);
    
    expect(screen.getByText('💡 TRY IT YOURSELF:')).toBeDefined();
    expect(screen.getByText(/PAUSE THE TOUR AND INTERACT/)).toBeDefined();
  });

  it('should include GitHub link in share modal', async () => {
    render(<DemoMode isActive={true} />);
    
    const shareButton = screen.getByText('🔗 SHARE');
    fireEvent.click(shareButton);
    
    await waitFor(() => {
      expect(screen.getByText('⭐ STAR ON GITHUB')).toBeDefined();
    });
  });

  it('should close share modal when CLOSE button clicked', async () => {
    render(<DemoMode isActive={true} />);
    
    const shareButton = screen.getByText('🔗 SHARE');
    fireEvent.click(shareButton);
    
    await waitFor(() => {
      expect(screen.getByText('SHARE DEMO LINK')).toBeDefined();
    });
    
    const closeButton = screen.getAllByText('CLOSE')[0];
    fireEvent.click(closeButton);
    
    await waitFor(() => {
      expect(screen.queryByText('SHARE DEMO LINK')).toBeNull();
    });
  });

  it('should call onAutoPlayComplete when auto-play finishes', async () => {
    vi.useFakeTimers();
    const onAutoPlayComplete = vi.fn();
    
    render(<DemoMode isActive={true} onAutoPlayComplete={onAutoPlayComplete} />);
    
    const autoPlayButton = screen.getByText('▶ AUTO-PLAY');
    fireEvent.click(autoPlayButton);
    
    // FAST-FORWARD TO END OF AUTO-PLAY (36 SECONDS)
    vi.advanceTimersByTime(36000);
    
    await waitFor(() => {
      expect(onAutoPlayComplete).toHaveBeenCalled();
    });
    
    vi.useRealTimers();
  });
});

describe('Demo Mode Integration', () => {
  it('should highlight target elements during tour', () => {
    const mockElement = {
      classList: {
        add: vi.fn(),
        remove: vi.fn()
      },
      getBoundingClientRect: () => ({
        top: 100,
        left: 100,
        bottom: 200,
        right: 200,
        width: 100,
        height: 100
      }),
      scrollIntoView: vi.fn()
    };
    
    document.getElementById = vi.fn(() => mockElement);
    document.querySelectorAll = vi.fn(() => [mockElement]);
    
    render(<DemoMode isActive={true} />);
    
    expect(mockElement.classList.add).toHaveBeenCalledWith('demo-highlight');
  });

  it('should remove highlights when demo mode closes', () => {
    const mockElement = {
      classList: {
        add: vi.fn(),
        remove: vi.fn()
      },
      getBoundingClientRect: () => ({
        top: 100,
        left: 100,
        bottom: 200,
        right: 200,
        width: 100,
        height: 100
      }),
      scrollIntoView: vi.fn()
    };
    
    document.getElementById = vi.fn(() => mockElement);
    document.querySelectorAll = vi.fn(() => [mockElement]);
    
    const onClose = vi.fn();
    render(<DemoMode isActive={true} onClose={onClose} />);
    
    const closeButton = screen.getByText('✕');
    fireEvent.click(closeButton);
    
    expect(mockElement.classList.remove).toHaveBeenCalledWith('demo-highlight');
    expect(onClose).toHaveBeenCalled();
  });

  it('should scroll target element into view', () => {
    const mockElement = {
      classList: {
        add: vi.fn(),
        remove: vi.fn()
      },
      getBoundingClientRect: () => ({
        top: 100,
        left: 100,
        bottom: 200,
        right: 200,
        width: 100,
        height: 100
      }),
      scrollIntoView: vi.fn()
    };
    
    document.getElementById = vi.fn(() => mockElement);
    document.querySelectorAll = vi.fn(() => []);
    
    render(<DemoMode isActive={true} />);
    
    expect(mockElement.scrollIntoView).toHaveBeenCalledWith({
      behavior: 'smooth',
      block: 'center'
    });
  });
});

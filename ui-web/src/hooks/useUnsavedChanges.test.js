import { renderHook, act } from '@testing-library/react';
import { useUnsavedChanges, useLogoutWithConfirmation } from './useUnsavedChanges';

describe('useUnsavedChanges', () => {
  let originalAddEventListener;
  let originalRemoveEventListener;
  let originalConfirm;

  beforeEach(() => {
    // Mock window methods
    originalAddEventListener = window.addEventListener;
    originalRemoveEventListener = window.removeEventListener;
    originalConfirm = window.confirm;

    window.addEventListener = jest.fn();
    window.removeEventListener = jest.fn();
    window.confirm = jest.fn();
  });

  afterEach(() => {
    // Restore original methods
    window.addEventListener = originalAddEventListener;
    window.removeEventListener = originalRemoveEventListener;
    window.confirm = originalConfirm;
    jest.clearAllMocks();
  });

  describe('useUnsavedChanges hook', () => {
    it('should initialize with no unsaved changes', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      const currentData = { name: 'John', email: 'john@example.com' };

      const { result } = renderHook(() => 
        useUnsavedChanges(currentData, originalData)
      );

      expect(result.current.hasUnsavedChanges).toBe(false);
    });

    it('should detect unsaved changes when current data differs from original', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      const currentData = { name: 'Jane', email: 'john@example.com' };

      const { result } = renderHook(() => 
        useUnsavedChanges(currentData, originalData)
      );

      expect(result.current.hasUnsavedChanges).toBe(true);
    });

    it('should update unsaved changes when current data changes', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      let currentData = { name: 'John', email: 'john@example.com' };

      const { result, rerender } = renderHook(
        ({ current, original }) => useUnsavedChanges(current, original),
        { initialProps: { current: currentData, original: originalData } }
      );

      expect(result.current.hasUnsavedChanges).toBe(false);

      // Change current data
      currentData = { name: 'Jane', email: 'john@example.com' };
      rerender({ current: currentData, original: originalData });

      expect(result.current.hasUnsavedChanges).toBe(true);

      // Revert changes
      currentData = { name: 'John', email: 'john@example.com' };
      rerender({ current: currentData, original: originalData });

      expect(result.current.hasUnsavedChanges).toBe(false);
    });

    it('should not detect changes when disabled', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      const currentData = { name: 'Jane', email: 'john@example.com' };

      const { result } = renderHook(() => 
        useUnsavedChanges(currentData, originalData, false)
      );

      expect(result.current.hasUnsavedChanges).toBe(false);
    });

    it('should clear unsaved changes when disabled', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      const currentData = { name: 'Jane', email: 'john@example.com' };

      const { result, rerender } = renderHook(
        ({ current, original, enabled }) => useUnsavedChanges(current, original, enabled),
        { initialProps: { current: currentData, original: originalData, enabled: true } }
      );

      expect(result.current.hasUnsavedChanges).toBe(true);

      // Disable the hook
      rerender({ current: currentData, original: originalData, enabled: false });

      expect(result.current.hasUnsavedChanges).toBe(false);
    });

    it('should update original data reference when props change', () => {
      let originalData = { name: 'John', email: 'john@example.com' };
      const currentData = { name: 'Jane', email: 'john@example.com' };

      const { result, rerender } = renderHook(
        ({ current, original }) => useUnsavedChanges(current, original),
        { initialProps: { current: currentData, original: originalData } }
      );

      expect(result.current.hasUnsavedChanges).toBe(true);

      // Update original data to match current
      originalData = { name: 'Jane', email: 'john@example.com' };
      rerender({ current: currentData, original: originalData });

      expect(result.current.hasUnsavedChanges).toBe(false);
    });

    it('should handle complex nested objects', () => {
      const originalData = {
        user: { name: 'John', preferences: { theme: 'dark' } },
        settings: { notifications: true }
      };
      const currentData = {
        user: { name: 'John', preferences: { theme: 'light' } },
        settings: { notifications: true }
      };

      const { result } = renderHook(() => 
        useUnsavedChanges(currentData, originalData)
      );

      expect(result.current.hasUnsavedChanges).toBe(true);
    });

    it('should handle null and undefined values', () => {
      const originalData = { name: 'John', email: null };
      const currentData = { name: 'John', email: undefined };

      const { result } = renderHook(() => 
        useUnsavedChanges(currentData, originalData)
      );

      expect(result.current.hasUnsavedChanges).toBe(true);
    });
  });

  describe('beforeunload event handling', () => {
    it('should add beforeunload listener when there are unsaved changes', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      const currentData = { name: 'Jane', email: 'john@example.com' };

      renderHook(() => useUnsavedChanges(currentData, originalData));

      expect(window.addEventListener).toHaveBeenCalledWith('beforeunload', expect.any(Function));
    });

    it('should remove beforeunload listener when no unsaved changes', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      let currentData = { name: 'Jane', email: 'john@example.com' };

      const { rerender } = renderHook(
        ({ current, original }) => useUnsavedChanges(current, original),
        { initialProps: { current: currentData, original: originalData } }
      );

      expect(window.addEventListener).toHaveBeenCalledWith('beforeunload', expect.any(Function));

      // Revert changes
      currentData = { name: 'John', email: 'john@example.com' };
      rerender({ current: currentData, original: originalData });

      expect(window.removeEventListener).toHaveBeenCalledWith('beforeunload', expect.any(Function));
    });

    it('should not add beforeunload listener when disabled', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      const currentData = { name: 'Jane', email: 'john@example.com' };

      renderHook(() => useUnsavedChanges(currentData, originalData, false));

      expect(window.addEventListener).not.toHaveBeenCalledWith('beforeunload', expect.any(Function));
    });

    it('should remove beforeunload listener on unmount', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      const currentData = { name: 'Jane', email: 'john@example.com' };

      const { unmount } = renderHook(() => useUnsavedChanges(currentData, originalData));

      unmount();

      expect(window.removeEventListener).toHaveBeenCalledWith('beforeunload', expect.any(Function));
    });

    it('should prevent default and set returnValue in beforeunload handler', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      const currentData = { name: 'Jane', email: 'john@example.com' };

      renderHook(() => useUnsavedChanges(currentData, originalData));

      const beforeunloadHandler = window.addEventListener.mock.calls
        .find(call => call[0] === 'beforeunload')[1];

      const mockEvent = {
        preventDefault: jest.fn(),
        returnValue: undefined
      };

      const result = beforeunloadHandler(mockEvent);

      expect(mockEvent.preventDefault).toHaveBeenCalled();
      expect(mockEvent.returnValue).toBe('');
      expect(result).toBe('');
    });

    it('should not prevent default when disabled in beforeunload handler', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      const currentData = { name: 'Jane', email: 'john@example.com' };

      renderHook(() => useUnsavedChanges(currentData, originalData, false));

      // Simulate the beforeunload handler being called (though it shouldn't be added)
      const mockEvent = {
        preventDefault: jest.fn(),
        returnValue: undefined
      };

      // Since the event listener shouldn't be added when disabled, this test ensures
      // the internal logic handles the enabled state correctly
      expect(window.addEventListener).not.toHaveBeenCalledWith('beforeunload', expect.any(Function));
    });
  });

  describe('confirmNavigation function', () => {
    it('should return true when no unsaved changes', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      const currentData = { name: 'John', email: 'john@example.com' };

      const { result } = renderHook(() => 
        useUnsavedChanges(currentData, originalData)
      );

      const confirmed = result.current.confirmNavigation();
      expect(confirmed).toBe(true);
      expect(window.confirm).not.toHaveBeenCalled();
    });

    it('should show confirm dialog when there are unsaved changes', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      const currentData = { name: 'Jane', email: 'john@example.com' };

      window.confirm.mockReturnValue(true);

      const { result } = renderHook(() => 
        useUnsavedChanges(currentData, originalData)
      );

      const confirmed = result.current.confirmNavigation();
      expect(confirmed).toBe(true);
      expect(window.confirm).toHaveBeenCalledWith('You have unsaved changes. Are you sure you want to logout?');
    });

    it('should use custom message in confirm dialog', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      const currentData = { name: 'Jane', email: 'john@example.com' };

      window.confirm.mockReturnValue(false);

      const { result } = renderHook(() => 
        useUnsavedChanges(currentData, originalData)
      );

      const customMessage = 'Custom unsaved changes message';
      const confirmed = result.current.confirmNavigation(customMessage);
      expect(confirmed).toBe(false);
      expect(window.confirm).toHaveBeenCalledWith(customMessage);
    });

    it('should return false when user cancels confirmation', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      const currentData = { name: 'Jane', email: 'john@example.com' };

      window.confirm.mockReturnValue(false);

      const { result } = renderHook(() => 
        useUnsavedChanges(currentData, originalData)
      );

      const confirmed = result.current.confirmNavigation();
      expect(confirmed).toBe(false);
    });
  });

  describe('setOriginalData function', () => {
    it('should update original data and clear unsaved changes', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      const currentData = { name: 'Jane', email: 'john@example.com' };

      const { result } = renderHook(() => 
        useUnsavedChanges(currentData, originalData)
      );

      expect(result.current.hasUnsavedChanges).toBe(true);

      act(() => {
        result.current.setOriginalData(currentData);
      });

      expect(result.current.hasUnsavedChanges).toBe(false);
    });

    it('should detect new changes after setting original data', () => {
      const originalData = { name: 'John', email: 'john@example.com' };
      let currentData = { name: 'Jane', email: 'john@example.com' };

      const { result, rerender } = renderHook(
        ({ current, original }) => useUnsavedChanges(current, original),
        { initialProps: { current: currentData, original: originalData } }
      );

      expect(result.current.hasUnsavedChanges).toBe(true);

      // Set original data to current
      act(() => {
        result.current.setOriginalData(currentData);
      });

      expect(result.current.hasUnsavedChanges).toBe(false);

      // Make new changes
      currentData = { name: 'Bob', email: 'john@example.com' };
      rerender({ current: currentData, original: originalData });

      expect(result.current.hasUnsavedChanges).toBe(true);
    });
  });
});

describe('useLogoutWithConfirmation', () => {
  let originalConfirm;
  let originalConsoleError;

  beforeEach(() => {
    originalConfirm = window.confirm;
    originalConsoleError = console.error;
    
    window.confirm = jest.fn();
    console.error = jest.fn();
  });

  afterEach(() => {
    window.confirm = originalConfirm;
    console.error = originalConsoleError;
    jest.clearAllMocks();
  });

  it('should call logout function immediately when no unsaved changes', async () => {
    const mockLogout = jest.fn().mockResolvedValue();
    const hasUnsavedChanges = false;

    const { result } = renderHook(() => 
      useLogoutWithConfirmation(mockLogout, hasUnsavedChanges)
    );

    const logoutResult = await result.current();

    expect(logoutResult).toBe(true);
    expect(mockLogout).toHaveBeenCalled();
    expect(window.confirm).not.toHaveBeenCalled();
  });

  it('should show confirmation dialog when there are unsaved changes', async () => {
    const mockLogout = jest.fn().mockResolvedValue();
    const hasUnsavedChanges = true;

    window.confirm.mockReturnValue(true);

    const { result } = renderHook(() => 
      useLogoutWithConfirmation(mockLogout, hasUnsavedChanges)
    );

    const logoutResult = await result.current();

    expect(logoutResult).toBe(true);
    expect(window.confirm).toHaveBeenCalledWith('You have unsaved changes. Are you sure you want to logout?');
    expect(mockLogout).toHaveBeenCalled();
  });

  it('should not call logout when user cancels confirmation', async () => {
    const mockLogout = jest.fn().mockResolvedValue();
    const hasUnsavedChanges = true;

    window.confirm.mockReturnValue(false);

    const { result } = renderHook(() => 
      useLogoutWithConfirmation(mockLogout, hasUnsavedChanges)
    );

    const logoutResult = await result.current();

    expect(logoutResult).toBe(false);
    expect(window.confirm).toHaveBeenCalledWith('You have unsaved changes. Are you sure you want to logout?');
    expect(mockLogout).not.toHaveBeenCalled();
  });

  it('should handle logout function errors', async () => {
    const mockError = new Error('Logout failed');
    const mockLogout = jest.fn().mockRejectedValue(mockError);
    const hasUnsavedChanges = false;

    const { result } = renderHook(() => 
      useLogoutWithConfirmation(mockLogout, hasUnsavedChanges)
    );

    await expect(result.current()).rejects.toThrow('Logout failed');
    expect(console.error).toHaveBeenCalledWith('Logout failed:', mockError);
  });

  it('should handle logout function errors with confirmation', async () => {
    const mockError = new Error('Logout failed');
    const mockLogout = jest.fn().mockRejectedValue(mockError);
    const hasUnsavedChanges = true;

    window.confirm.mockReturnValue(true);

    const { result } = renderHook(() => 
      useLogoutWithConfirmation(mockLogout, hasUnsavedChanges)
    );

    await expect(result.current()).rejects.toThrow('Logout failed');
    expect(window.confirm).toHaveBeenCalled();
    expect(console.error).toHaveBeenCalledWith('Logout failed:', mockError);
  });

  it('should update when dependencies change', async () => {
    let mockLogout = jest.fn().mockResolvedValue();
    let hasUnsavedChanges = false;

    const { result, rerender } = renderHook(
      ({ logout, unsaved }) => useLogoutWithConfirmation(logout, unsaved),
      { initialProps: { logout: mockLogout, unsaved: hasUnsavedChanges } }
    );

    // Initial call without confirmation
    await result.current();
    expect(mockLogout).toHaveBeenCalledTimes(1);
    expect(window.confirm).not.toHaveBeenCalled();

    // Update to have unsaved changes
    const newMockLogout = jest.fn().mockResolvedValue();
    hasUnsavedChanges = true;
    window.confirm.mockReturnValue(true);

    rerender({ logout: newMockLogout, unsaved: hasUnsavedChanges });

    // New call should show confirmation
    await result.current();
    expect(window.confirm).toHaveBeenCalled();
    expect(newMockLogout).toHaveBeenCalledTimes(1);
    expect(mockLogout).toHaveBeenCalledTimes(1); // Old function not called again
  });
});
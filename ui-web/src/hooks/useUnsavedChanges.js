import { useEffect, useRef, useState, useCallback } from 'react';

/**
 * Hook to detect unsaved changes in forms and manage warning dialogs
 * @param {Object} currentData - Current form data
 * @param {Object} originalData - Original form data for comparison
 * @param {boolean} enabled - Whether to enable unsaved changes detection
 * @returns {Object} - { hasUnsavedChanges, confirmNavigation, setOriginalData }
 */
export const useUnsavedChanges = (currentData, originalData, enabled = true) => {
  const [hasUnsavedChanges, setHasUnsavedChanges] = useState(false);
  const originalDataRef = useRef(originalData);
  const enabledRef = useRef(enabled);

  // Update refs when props change
  useEffect(() => {
    originalDataRef.current = originalData;
    enabledRef.current = enabled;
  }, [originalData, enabled]);

  // Check for changes whenever currentData changes
  useEffect(() => {
    if (!enabledRef.current) {
      setHasUnsavedChanges(false);
      return;
    }

    const hasChanges = JSON.stringify(currentData) !== JSON.stringify(originalDataRef.current);
    setHasUnsavedChanges(hasChanges);
  }, [currentData]);

  // Set up beforeunload event listener
  useEffect(() => {
    const handleBeforeUnload = (e) => {
      if (hasUnsavedChanges && enabledRef.current) {
        e.preventDefault();
        e.returnValue = '';
        return '';
      }
    };

    if (hasUnsavedChanges && enabled) {
      window.addEventListener('beforeunload', handleBeforeUnload);
    }

    return () => {
      window.removeEventListener('beforeunload', handleBeforeUnload);
    };
  }, [hasUnsavedChanges, enabled]);

  // Function to confirm navigation with unsaved changes
  const confirmNavigation = useCallback((message = 'You have unsaved changes. Are you sure you want to logout?') => {
    if (!hasUnsavedChanges) {
      return true;
    }
    return window.confirm(message);
  }, [hasUnsavedChanges]);

  // Function to update the original data (after save)
  const setOriginalData = useCallback((newOriginalData) => {
    originalDataRef.current = newOriginalData;
    setHasUnsavedChanges(false);
  }, []);

  return {
    hasUnsavedChanges,
    confirmNavigation,
    setOriginalData
  };
};

/**
 * Hook to create a logout confirmation dialog with unsaved changes warning
 * @param {Function} logoutFn - The logout function to call
 * @param {boolean} hasUnsavedChanges - Whether there are unsaved changes
 * @returns {Function} - Enhanced logout function with confirmation
 */
export const useLogoutWithConfirmation = (logoutFn, hasUnsavedChanges) => {
  return useCallback(async () => {
    if (hasUnsavedChanges) {
      const confirmed = window.confirm('You have unsaved changes. Are you sure you want to logout?');
      if (!confirmed) {
        return false; // User cancelled logout
      }
    }
    
    try {
      await logoutFn();
      return true; // Logout successful
    } catch (error) {
      console.error('Logout failed:', error);
      throw error;
    }
  }, [logoutFn, hasUnsavedChanges]);
};

export default useUnsavedChanges;
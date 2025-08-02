// Constants for session durations
const STANDARD_SESSION_DURATION = 24 * 60 * 60 * 1000; // 24 hours in milliseconds
const EXTENDED_SESSION_DURATION = 30 * 24 * 60 * 60 * 1000; // 30 days in milliseconds
const REMEMBER_ME_KEY = 'rememberMe';

/**
 * Set the remember me flag in localStorage
 * @param {boolean} rememberMe - Whether to remember the user
 */
export const setRememberMeFlag = (rememberMe) => {
  localStorage.setItem(REMEMBER_ME_KEY, rememberMe.toString());
};

/**
 * Get the remember me flag from localStorage
 * @returns {boolean} - Whether the user should be remembered
 */
export const getRememberMeFlag = () => {
  const flag = localStorage.getItem(REMEMBER_ME_KEY);
  return flag === 'true';
};

/**
 * Clear the remember me flag from localStorage
 */
export const clearRememberMeFlag = () => {
  localStorage.removeItem(REMEMBER_ME_KEY);
};

/**
 * Check if the current session is a remember me session
 * @returns {boolean} - Whether this is a remember me session
 */
export const isRememberMeSession = () => {
  return getRememberMeFlag();
};

/**
 * Decode JWT token to get payload
 * @param {string} token - JWT token
 * @returns {object|null} - Decoded payload or null if invalid
 */
const decodeJWTPayload = (token) => {
  try {
    if (!token) return null;
    
    const parts = token.split('.');
    if (parts.length !== 3) return null;
    
    const payload = parts[1];
    const decoded = JSON.parse(atob(payload));
    return decoded;
  } catch (error) {
    console.error('Error decoding JWT:', error);
    return null;
  }
};

/**
 * Get token expiry time based on remember me setting
 * @param {string} token - JWT access token
 * @param {boolean} isRememberMe - Whether this is a remember me session
 * @returns {number} - Expiry timestamp in milliseconds
 */
export const getTokenExpiryTime = (token, isRememberMe) => {
  // First try to get expiry from token itself
  const payload = decodeJWTPayload(token);
  if (payload && payload.exp) {
    return payload.exp * 1000; // Convert from seconds to milliseconds
  }
  
  // If no exp claim, use our own duration logic
  const currentTime = Date.now();
  const duration = isRememberMe ? EXTENDED_SESSION_DURATION : STANDARD_SESSION_DURATION;
  return currentTime + duration;
};

/**
 * Check if a token is expired
 * @param {number} expiryTime - Expiry timestamp in milliseconds
 * @returns {boolean} - Whether the token is expired
 */
export const isTokenExpired = (expiryTime) => {
  if (!expiryTime) return true;
  return Date.now() >= expiryTime;
};

/**
 * Store token expiry time in localStorage
 * @param {number} expiryTime - Expiry timestamp in milliseconds
 */
export const storeTokenExpiry = (expiryTime) => {
  localStorage.setItem('tokenExpiry', expiryTime.toString());
};

/**
 * Get stored token expiry time from localStorage
 * @returns {number|null} - Expiry timestamp in milliseconds or null
 */
export const getStoredTokenExpiry = () => {
  const expiry = localStorage.getItem('tokenExpiry');
  return expiry ? parseInt(expiry, 10) : null;
};

/**
 * Clear stored token expiry from localStorage
 */
export const clearStoredTokenExpiry = () => {
  localStorage.removeItem('tokenExpiry');
};
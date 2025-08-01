// Application configuration
const config = {
  api: {
    // Use relative URL so React proxy can work in development
    baseURL: process.env.REACT_APP_API_URL || '/api/v1',
    timeout: parseInt(process.env.REACT_APP_API_TIMEOUT) || 30000,
  },
  auth: {
    tokenRefreshThreshold: 5 * 60 * 1000, // 5 minutes before expiry
  },
  query: {
    staleTime: 5 * 60 * 1000, // 5 minutes
    cacheTime: 10 * 60 * 1000, // 10 minutes
    refetchOnWindowFocus: false,
    retry: 1,
  },
  pagination: {
    defaultPageSize: 20,
    pageSizeOptions: [10, 20, 50, 100],
  },
};

export default config;
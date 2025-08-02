const config = {
  api: {
    baseURL: process.env.REACT_APP_API_URL || '/api/v1',
    timeout: 30000,
  },
  query: {
    retry: 3,
    retryDelay: 1000,
    staleTime: 5 * 60 * 1000, // 5 minutes
    cacheTime: 10 * 60 * 1000, // 10 minutes
  },
};

export default config;
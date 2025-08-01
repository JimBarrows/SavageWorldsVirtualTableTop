const webpack = require('webpack');

module.exports = {
  webpack: {
    plugins: [
      new webpack.ProvidePlugin({
        process: 'process/browser',
      }),
      new webpack.DefinePlugin({
        'process.env': JSON.stringify(process.env)
      })
    ],
    configure: (webpackConfig) => {
      // Add fallbacks for webpack 5 (if using webpack 5)
      if (webpackConfig.resolve.fallback) {
        webpackConfig.resolve.fallback = {
          ...webpackConfig.resolve.fallback,
          "process": require.resolve("process/browser")
        };
      }
      
      return webpackConfig;
    }
  },
  jest: {
    configure: {
      moduleNameMapper: {
        '^axios$': require.resolve('axios'),
      },
      transformIgnorePatterns: [
        'node_modules/(?!(axios)/)'
      ]
    }
  }
};
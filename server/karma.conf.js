module.exports = function (config) {
	config.set({

		basePath: './app',

		files: [
			'models/*.js'
			, 'public/**/*'
			, 'routes/*.js'
			, 'test/*.js'
			, 'views/*.jade'
		],

		autoWatch: true,

		frameworks: ['jasmine'],

		browsers: ['Chrome'],

		plugins: [
			'karma-chrome-launcher',
			'karma-firefox-launcher',
			'karma-jasmine',
			'karma-junit-reporter'
		],

		junitReporter: {
			outputFile: 'test_out/unit.xml',
			suite: 'unit'
		}

	});
};
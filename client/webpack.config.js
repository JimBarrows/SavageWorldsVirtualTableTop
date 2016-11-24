var debug   = process.env.NODE_ENV !== 'production';
var webpack = require('webpack');

module.exports = {
	context: __dirname + "/src"
	, devtool: debug ? "inline-sourcemap" : null
	, entry: ['./js/client.js']
	, module: {
		loaders: [
			{
				test: /\.js?$/,
				loader: 'babel',
				exclude: /(node_modules|bower_components)/,
				query: {
					presets: ['react', 'es2015', 'stage-2'],
					plugins: ['react-html-attrs', 'transform-class-properties', 'transform-decorators-legacy']
				}
			}
			, {test: /\.css$/, loader: "style-loader!css-loader"}
			, {test: /\.eot(\?v=\d+\.\d+\.\d+)?$/, loader: "file"}
			, {test: /\.(woff|woff2)$/, loader: "url?prefix=font/&limit=5000"}
			, {test: /\.ttf(\?v=\d+\.\d+\.\d+)?$/, loader: "url?limit=10000&mimetype=application/octet-stream"}
			, {test: /\.svg(\?v=\d+\.\d+\.\d+)?$/, loader: "url?limit=10000&mimetype=image/svg+xml"}
			, {test: /\.json$/, loader: 'json-loader'}
			, {
				test: /\.scss$/,
				exclude: /node_modules/,
				loader: 'style!css?modules&importLoaders=2&sourceMap&localIdentName=[local]___[hash:base64:5]!autoprefixer?browsers=last 2 version!sass?outputStyle=expanded&sourceMap&includePaths[]=node_modules/compass-mixins/lib'
			}
		]
	}
	, output: {
		path: __dirname + "/src/"
		, filename: "client.min.js"
	}
	, plugins: debug ? [] : [
		new webpack.optimize.DedupePlugin()
		, new webpack.optimize.OccurenceOrderPlugin()
		, new webpack.optimize.UglifyPlugin({mangle: false, sourcemap: false})
	]
	, devServer: {
		contentBase: "src"
		, hot: true
		, proxy: {
			'/api/*': {
				target: 'http://0.0.0.0:3000/',
				secure: false
			}
		}
	}
};
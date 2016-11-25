var path    = require("path");
var webpack = require("webpack");

var definePlugin = new webpack.DefinePlugin({
	__DEV__: JSON.stringify(JSON.parse(process.env.BUILD_DEV || 'true')),
	__PRERELEASE__: JSON.stringify(JSON.parse(process.env.BUILD_PRERELEASE || 'false'))
});

module.exports = {
	context: __dirname + "/src",
	devtool: "eval-source-map",
	entry: {main: "./js/client.jsx"},
	output: {
		path: __dirname + "/src/"
		, filename: "client.min.js"
	},
	plugins: [
		definePlugin
		// new webpack.optimize.DedupePlugin()
		// , new webpack.optimize.OccurenceOrderPlugin()
		// , new webpack.optimize.UglifyPlugin({mangle: false, sourcemap: false})
	],
	module: {
		loaders: [
			{
				test: /\.jsx?$/,
				loader: "babel",
				exclude: /(node_modules|bower_components)/,
				query: {
					presets: ["react", "es2015", "stage-2"],
					plugins: ["react-html-attrs", "transform-class-properties", "transform-decorators-legacy"]
				}
			},
			{
				test: /\.js?$/,
				loader: "babel",
				exclude: /(node_modules|bower_components)/,
				query: {
					presets: ["react", "es2015", "stage-2"],
					plugins: ["react-html-attrs", "transform-class-properties", "transform-decorators-legacy"]
				}
			}
			, {test: /\.css$/, loader: "style-loader!css-loader"}
			, {
				test: /\.(ttf|eot|svg|woff(2)?)(\?v=\d+\.\d+\.\d+)?$/,
				loader: "url"
			}
			, {test: /\.svg(\?v=\d+\.\d+\.\d+)?$/, loader: "url?limit=10000&mimetype=image/svg+xml"}
			, {test: /\.json$/, loader: "json-loader"}
			, {
				test: /\.scss$/,
				exclude: /node_modules/,
				loader: "style!css?modules&importLoaders=2&sourceMap&localIdentName=[local]___[hash:base64:5]!autoprefixer?browsers=last 2 version!sass?outputStyle=expanded&sourceMap&includePaths[]=node_modules/compass-mixins/lib"
			}, {
				test: /\.json?$/,
				loader: "json"
			}
		]
	},
	resolve: {
		extensions: ["", ".js", ".jsx"],
	},
	devServer: {
		contentBase: "src"
		, hot: true
		, proxy: {
			"/api/*": {
				target: "http://0.0.0.0:3000/",
				secure: false
			}
		}
	}
};
module.exports = function (api) {
	api.cache(true)

	const presets = ["@babel/preset-env",
									 "@babel/preset-react"]
	const plugins = ["css-modules-transform",
									 "transform-react-jsx",
									 "transform-class-properties",
									 "react-html-attrs",
									 "@babel/plugin-proposal-nullish-coalescing-operator",
									 "@babel/plugin-proposal-class-properties",
									 "@babel/plugin-proposal-private-methods"]

	return {
		presets,
		plugins
	}
}

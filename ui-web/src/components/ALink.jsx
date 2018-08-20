import PropTypes from 'prop-types'
import React from 'react'

export default class ALink extends React.Component {

	static defaultProps = {}

	static propTypes = {
		className: PropTypes.string,
		id       : PropTypes.string.isRequired,
		href     : PropTypes.string,
		onClick  : PropTypes.func
	}

	render() {
		let {className, href, id, onClick} = this.props
		let componentId                    = `ALink-${id}`
		let attributes                     = {
			className
		}
		if (href) {
			attributes = Object.assign({}, {href}, attributes)
		} else if (onClick) {
			attributes = Object.assign({}, {onclick: onClick}, attributes)
		} else {
			attributes = Object.assign({}, {href: '#'}, attributes)
		}
		return (
			<a id={componentId} {...attributes}>
				{this.props.children}
			</a>
		)
	}
}


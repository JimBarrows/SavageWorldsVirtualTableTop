import PropTypes from 'prop-types'
import React from 'react'

export default class ALink extends React.Component {

	static defaultProps = {}

	static propTypes = {
		className: PropTypes.string,
		id       : PropTypes.string.isRequired,
		onClick  : PropTypes.func,
		children : PropTypes.any.isRequired
	}

	onClick = e => {
		e.preventDefault()
		this.props.onClick(e)
	}


	render() {
		let {className, id} = this.props
		let componentId     = `ALink-${id}`
		return (
			<a id={componentId} href={'#'} onClick={this.onClick} className={className}>
				{this.props.children}
			</a>
		)
	}
}


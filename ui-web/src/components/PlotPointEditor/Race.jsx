import PropTypes from 'prop-types'
import React from 'react'

export default class Race extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id: PropTypes.string.isRequired
	}

	render() {
		let {id}        = this.props
		let componentId = `Race-${id}`
		return (
			<div id={component_id}>
				Race component
			</div>
		)
	}
}


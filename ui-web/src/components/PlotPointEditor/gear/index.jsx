import PropTypes from 'prop-types'
import React     from 'react'

export default class Gear extends React.Component {

	static propTypes = {
		id: PropTypes.string.isRequired
	}

	static defaultProps = {}

	render () {
		return (
			<div id={'gearComponent_' + this.props.id} >
				<h1 >Gear</h1 >
			</div >
		)
	}
}


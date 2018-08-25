import PropTypes from 'prop-types'
import React from 'react'
import SkillsList from '../lists/Skills'

export default class Skills extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id: PropTypes.string.isRequired,
		onChange: PropTypes.func.isRequired,
		plotPoint: PropTypes.object.isRequired
	}



	render() {
		let {id, plotPoint} = this.props
		let component_id = `Skills-${id}`
		return (<div id={component_id}>
			<h1>Setting Rules</h1>
			<SkillsList id={`${component_id}`} onChange={this.SkillsChange} rules={plotPoint.Skills}/>
		</div>)
	}
}

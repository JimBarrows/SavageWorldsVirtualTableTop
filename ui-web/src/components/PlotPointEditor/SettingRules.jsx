import PropTypes from 'prop-types'
import React from 'react'
import SettingRulesList from '../lists/SettingRules'

export default class SettingRules extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id       : PropTypes.string.isRequired,
		onChange : PropTypes.func.isRequired,
		plotPoint: PropTypes.object.isRequired
	}

	settingRulesChange = settingRules => this.props.onChange(Object.assign({}, this.props.plotPoint, {settingRules: settingRules}))

	render() {
		let {id, plotPoint} = this.props
		let component_id    = `SettingRules-${id}`
		return (
			<div id={component_id}>
				<h1>Setting Rules</h1>
				<SettingRulesList id={`${component_id}`} onChange={this.settingRulesChange} rules={plotPoint.settingRules}/>
			</div>
		)
	}
}


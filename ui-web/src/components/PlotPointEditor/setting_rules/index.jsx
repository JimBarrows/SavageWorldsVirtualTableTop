import {Button, ListGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import Editor from './Editor'

export default class SettingRulesList extends React.Component {

	static defaultProps = {
		rules: []
	}

	static propTypes = {
		onChange: PropTypes.func.isRequired,
		rules   : PropTypes.arrayOf(PropTypes.shape({
			name       : PropTypes.string.isRequired,
			description: PropTypes.string.isRequired
		})),
		id      : PropTypes.string.isRequired
	}

	addSettingRule = (e) => {
		e.preventDefault()
		this.props.onChange([{name: ' ', description: ' '}, ...this.props.rules])
	}

	deleteSettingRule = settingRule => {
		let {rules, onChange} = this.props
		onChange(rules.filter(sr => sr.name !== settingRule.name && sr.description !== settingRule.description))
	}

	descriptionChange = indexOfChange => e => this.props.onChange(this.props.rules.map((rule, index) =>
		indexOfChange === index ? Object.assign({}, rule, {description: e.target.value}) : rule))

	nameChange = indexOfChange => e => this.props.onChange(this.props.rules.map((rule, index) =>
		indexOfChange === index ? Object.assign({}, rule, {name: e.target.value}) : rule))

	render() {
		let {rules, id}  = this.props
		let component_id = `SettingRules-${id}`
		return <div id={component_id}>
			<Button id={component_id} onClick={this.addSettingRule}>Add</Button>
			<ListGroup id={component_id} context={'light'}>
				{rules.map((a, i) => <Editor key={i} description={a.description}
				                             descriptionChange={this.descriptionChange(i)}
				                             id={`${component_id}-${i}`} name={a.name}
				                             nameChange={this.nameChange(i)}
				                             onDelete={this.deleteSettingRule}/>)}
			</ListGroup>

		</div>
	}
}

import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React                                               from 'react'
import BaseEditor                                          from '../../../BaseEditor'
import AttributeSelectFormGroup                            from '../../../formgroups/AttributeSelectFormGroup'

export default class Editor extends React.Component {

	attributeChange           = e => this.props.onChange(Object.assign({}, this.props.item, {attribute: e.target.value}), this.props.index)
	descriptionChange         = e => this.props.onChange(Object.assign({}, this.props.item, {description: e.target.value}), this.props.index)
	nameChange                = e => this.props.onChange(Object.assign({}, this.props.item, {name: e.target.value}), this.props.index)
	onDelete                  = event => {
		event.preventDefault()
		this.props.onDelete(this.props.index)
	}
	skillNameChange           = e => this.props.onChange(Object.assign({}, this.props.item, {skillName: e.target.value}), this.props.index)
	startingPowersChange      = e => this.props.onChange(Object.assign({}, this.props.item, {startingPowers: parseInt(e.target.value, 10) || 0}), this.props.index)
	startingPowerPointsChange = e => this.props.onChange(Object.assign({}, this.props.item, {startingPowerPoints: parseInt(e.target.value, 10) || 0}), this.props.index)

	render() {
		let componentId = `ArcaneBackgroundEditor-${this.props.index}-${this.props.id}`
		return (
			<BaseEditor id={this.props.id} onDelete={this.onDelete}>
				<TextFormGroup id={`${componentId}-Name`} label='Name' onChange={this.nameChange} required={true}
				               value={this.props.item.name}/>
				<TextAreaFormGroup id={`${componentId}-Description`}
				                   label="Description"
				                   onChange={this.descriptionChange}
				                   value={this.props.item.description}/>
				<TextFormGroup id={`${componentId}-Skill`} label='Skill' onChange={this.skillNameChange} required={true}
				               value={this.props.item.skillName}/>
				<AttributeSelectFormGroup id={`${componentId}-SkillAttribute`}
				                          onChange={this.attributeChange}
				                          attribute={this.props.item.attribute}/>
				<NumberFormGroup id={`${componentId}-StartingPowers`} label='Starting Powers'
				                 onChange={this.startingPowersChange}
				                 required={true}
				                 value={this.props.item.startingPowers}/>
				<NumberFormGroup id={`${componentId}-StartPowerPoints`} label='Starting Power Points'
				                 onChange={this.startingPowerPointsChange}
				                 required={true}
				                 value={this.props.item.startingPowerPoints}/>
			</BaseEditor>
		)
	}
}

Editor.propTypes = {
	onChange: PropTypes.func.isRequired,
	item: PropTypes.shape({
		name: PropTypes.string,
		description: PropTypes.string,
		skillName: PropTypes.string,
		attribute: PropTypes.string,
		startingPowers: PropTypes.oneOfType([PropTypes.string, PropTypes.number]),
		startingPowerPoints: PropTypes.oneOfType([PropTypes.string, PropTypes.number])
	}).isRequired,
	index: PropTypes.number.isRequired,
	onDelete: PropTypes.func.isRequired,
	id: PropTypes.string.isRequired
}

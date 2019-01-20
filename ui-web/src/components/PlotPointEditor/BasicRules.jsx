import {NumberFormGroup} from 'bootstrap-react-components'
import PropTypes         from 'prop-types'
import React             from 'react'

export default class BasicRules extends React.Component {

	static propTypes = {
		id        : PropTypes.string.isRequired,
		onChange  : PropTypes.func.isRequired,
		basicRules: PropTypes.shape({
																	maximumAttributePoints: PropTypes.number.isRequired,
																	maximumMajorHindrances: PropTypes.number.isRequired,
																	maximumMinorHindrances: PropTypes.number.isRequired,
																	maximumSkillPoints    : PropTypes.number.isRequired
																}).isRequired
	}

	static defaultProps = {
		basicRules: {
			maximumAttributePoints: 5,
			maximumMajorHindrances: 1,
			maximumMinorHindrances: 2,
			maximumSkillPoints    : 15
		},
	}

	maximumAttributePointsChange = e => this.props.onChange(Object.assign({}, this.props.basicRules, {maximumAttributePoints: parseInt(e.target.value, 10)}))
	maximumMajorHindrancesChange = e => this.props.onChange(Object.assign({}, this.props.basicRules, {maximumMajorHindrances: parseInt(e.target.value, 10)}))
	maximumMinorHindrancesChange = e => this.props.onChange(Object.assign({}, this.props.basicRules, {maximumMinorHindrances: parseInt(e.target.value, 10)}))
	maximumSkillPointsChange     = e => this.props.onChange(Object.assign({}, this.props.basicRules, {maximumSkillPoints: parseInt(e.target.value, 10)}))

	render () {
		let {id, basicRules} = this.props
		let componentId      = `BasicRulesComponent_-${id}`
		return (
			<div id={componentId} >
				<h1 >Basic Rules</h1 >
				<NumberFormGroup id={`${componentId}-MaximumAttributePoints`} label={'Maximum Attribute Points'}
					onChange={this.maximumAttributePointsChange} required={true}
					value={basicRules.maximumAttributePoints} />
				<NumberFormGroup id={`${componentId}-MaximumMajorHindrances`} label={'Maximum Number of Major Hindrances'}
					onChange={this.maximumMajorHindrancesChange} required={true}
					value={basicRules.maximumMajorHindrances} />
				<NumberFormGroup id={`${componentId}-MaximumMinorHindrances`} label={'Maximum Number of Minor Hindrances'}
					onChange={this.maximumMinorHindrancesChange} required={true}
					value={basicRules.maximumMinorHindrances} />
				<NumberFormGroup id={`${componentId}-MaximumSkillPoints`} label={'Maximum Skill Points'}
					onChange={this.maximumSkillPointsChange} required={true}
					value={basicRules.maximumSkillPoints} />
			</div >
		)
	}
}


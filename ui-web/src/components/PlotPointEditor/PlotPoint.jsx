import {NumberFormGroup, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import PropTypes                                           from 'prop-types'
import React                                               from 'react'
import Navigation                                          from './Navigation'

export default class PlotPoint extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id       : PropTypes.string.isRequired,
		onChange : PropTypes.func.isRequired,
		plotPoint: PropTypes.object.isRequired
	}

	descriptionChange            = e => this.props.onChange(Object.assign({}, this.props.plotPoint, {description: e.target.value}))
	maximumAttributePointsChange = e => this.props.onChange(Object.assign({}, this.props.plotPoint, {maximumAttributePoints: parseInt(e.target.value, 10)}))
	maximumMajorHindrancesChange = e => this.props.onChange(Object.assign({}, this.props.plotPoint, {maximumMajorHindrances: parseInt(e.target.value, 10)}))
	maximumMinorHindrancesChange = e => this.props.onChange(Object.assign({}, this.props.plotPoint, {maximumMinorHindrances: parseInt(e.target.value, 10)}))
	maximumSkillPointsChange     = e => this.props.onChange(Object.assign({}, this.props.plotPoint, {maximumSkillPoints: parseInt(e.target.value, 10)}))
	nameChange                   = e => this.props.onChange(Object.assign({}, this.props.plotPoint, {name: e.target.value}))
	showSection                  = section => console.log('section: ', section)

	render () {
		let {id, plotPoint} = this.props
		let componentId     = `PlotPoint-${id}`
		return (
			<div id={componentId} >
				<TextFormGroup id={`${componentId}-Name`} label='Name' onChange={this.nameChange} required={true}
					value={plotPoint.name} />
				<TextAreaFormGroup id={`${componentId}-Description`} label={'Description'} onChange={this.descriptionChange}
					value={plotPoint.description} />
				<Navigation id={componentId} navigateTo={this.showSection} />
				<h1 >Basic Rules</h1 >
				<NumberFormGroup id={`${componentId}-MaximumAttributePoints`} label={'Maximum Attribute Points'}
					onChange={this.maximumAttributePointsChange} required={true}
					value={plotPoint.maximumAttributePoints} />
				<NumberFormGroup id={`${componentId}-MaximumMajorHindrances`} label={'Maximum Number of Major Hindrances'}
					onChange={this.maximumMajorHindrancesChange} required={true}
					value={plotPoint.maximumMajorHindrances} />
				<NumberFormGroup id={`${componentId}-MaximumMinorHindrances`} label={'Maximum Number of Minor Hindrances'}
					onChange={this.maximumMinorHindrancesChange} required={true}
					value={plotPoint.maximumMinorHindrances} />
				<NumberFormGroup id={`${componentId}-MaximumSkillPoints`} label={'Maximum Skill Points'}
					onChange={this.maximumSkillPointsChange} required={true}
					value={plotPoint.maximumSkillPoints} />

			</div >
		)
	}
}


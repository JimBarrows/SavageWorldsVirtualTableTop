import {FontAwesomeIcon}                          from '@fortawesome/react-fontawesome'
import {Button, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import PropTypes                                  from 'prop-types'
import React                                      from 'react'
import BasicRules                                 from './BasicRules'
import Beasts                                     from './beasts'
import Characters                                 from './characters'
import Edges                                      from './edges'
import Gear                                       from './gear'
import Hindrances                                 from './hindrances'
import Navigation                                 from './Navigation'
import Powers                                     from './powers'
import Races                                      from './races'
import SettingRulesList                           from './setting_rules'
import Skills                                     from './skills'

export default class PlotPointForm extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id       : PropTypes.string.isRequired,
		onCancel : PropTypes.func.isRequired,
		onChange : PropTypes.func.isRequired,
		onSave   : PropTypes.func.isRequired,
		plotPoint: PropTypes.object.isRequired
	}

	basicRulesChange   = basicRules => this.props.onChange(Object.assign({}, this.props.plotPoint, {basicRules}))
	beastsChange       = beasts => this.props.onChange(Object.assign({}, this.props.plotPoint, {beasts}))
	charactersChange   = characters => this.props.onChange(Object.assign({}, this.props.plotPoint, {characters}))
	descriptionChange  = e => this.props.onChange(Object.assign({}, this.props.plotPoint, {description: e.target.value}))
	edgesChange        = edges => this.props.onChange(Object.assign({}, this.props.plotPoint, {edges}))
	gearChange         = gear => this.props.onChange(Object.assign({}, this.props.plotPoint, {gear}))
	hindrancesChange   = hindrances => this.props.onChange(Object.assign({}, this.props.plotPoint, {hindrances}))
	nameChange         = e => this.props.onChange(Object.assign({}, this.props.plotPoint, {name: e.target.value}))
	powersChange       = powers => this.props.onChange(Object.assign({}, this.props.plotPoint, {powers}))
	racesChange        = races => this.props.onChange(Object.assign({}, this.props.plotPoint, {races}))
	settingRulesChange = settingRules => this.props.onChange(Object.assign({}, this.props.plotPoint, {settingRules}))
	skillsChange       = skills => this.props.onChange(Object.assign({}, this.props.plotPoint, {skills}))

	sectionChange = section => this.setState({section})
	showSection   = (componentId) => {
		switch (this.state.section) {
			case 'BasicRules':
				return (<BasicRules plotPoint={this.props.plotPoint} onChange={this.basicRulesChange} id={componentId} />)
			case 'SettingRules':
				return (
					<SettingRulesList rules={this.props.plotPoint.rules} onChange={this.settingRulesChange} id={componentId} />)
			case 'Skills':
				return (
					<Skills rules={this.props.plotPoint.skills} onChange={this.skillsChange} id={componentId} />)
			case 'Edges':
				return (<Edges edges={this.props.plotPoint.edges} onChange={this.edgesChange} id={componentId} />)
			case 'Hindrances':
				return (
					<Hindrances edges={this.props.plotPoint.hindrances} onChange={this.hindrancesChange} id={componentId} />)
			case 'Gear':
				return (<Gear gear={this.props.plotPoint.gear} onChange={this.gearChange} id={componentId} />)
			case 'Powers':
				return (<Powers powers={this.props.plotPoint.powers} onChange={this.powersChange} id={componentId} />)
			case 'Races':
				return (<Races races={this.props.plotPoint.races} onChange={this.racesChange} id={componentId} />)
			case 'Beasts':
				return (<Beasts beasts={this.props.plotPoint.beasts} onChange={this.beastsChange} id={componentId} />)
			case 'Characters':
				return (
					<Characters characters={this.props.plotPoint.characters} onChange={this.charactersChange} id={componentId} />)
		}
	}

	render () {
		let {id, plotPoint} = this.props
		let componentId     = `PlotPoint-${id}`
		return (
			<form id={componentId} >
				<TextFormGroup id={`${componentId}-Name`} label='Name' onChange={this.nameChange} required={true}
					value={plotPoint.name} />
				<TextAreaFormGroup id={`${componentId}-Description`} label={'Description'} onChange={this.descriptionChange}
					value={plotPoint.description} />
				<div className={'row'} >
					<div className="col-2" >
						<Navigation id={componentId} navigateTo={this.sectionChange} />
					</div >
					<div className="col-10" >
						{this.showSection(componentId)}
					</div >
				</div >
				<div className="row" >
					<div className="col" >
					</div >
					<div className="col" >
						<Button id={'save-' + componentId} onClick={this.props.onSave} >
							<FontAwesomeIcon icon={'save'} />&nbsp;Save
						</Button >
						<Button id={'cancel-' + componentId} onClick={this.props.onCancel} >
							<FontAwesomeIcon icon={'ban'} />&nbsp;Cancel
						</Button >
					</div >
					<div className={'col'} >
					</div >
				</div >
			</form >
		)
	}

	state = {
		section: 'BasicRules'
	}
}


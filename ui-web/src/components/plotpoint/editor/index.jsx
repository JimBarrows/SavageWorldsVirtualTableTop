import {FontAwesomeIcon}                                 from '@fortawesome/react-fontawesome'
import {Alert, Button, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import {func, string, arrayOf, object, bool}            from 'prop-types'
import React                                             from 'react'
import PlotPointPropType                                 from '../../../propTypes/PlotPoint'
import BasicRules                                        from './BasicRules'
import Beasts                                            from './beasts'
import Characters                                        from './characters'
import Edges                                             from './edges'
import Gear                                              from './gear'
import Hindrances                                        from './hindrances'
import Navigation                                        from './Navigation'
import Powers                                            from './powers'
import Races                                             from './races'
import SettingRulesList                                  from './setting_rules'
import Skills                                            from './skills'

export default class PlotPointForm extends React.Component {

	static defaultProps = {
		errors: [],
		disabled: false
	}

	static propTypes = {
		id       : string.isRequired,
		onCancel : func.isRequired,
		onSave   : func.isRequired,
		plotPoint: PlotPointPropType.isRequired,
		errors   : arrayOf(object),
		disabled : bool
	}

	basicRulesChange   = basicRules => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {basicRules})})
	beastsChange       = beasts => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {beasts})})
	cancel             = e => {
		e.preventDefault()
		this.props.onCancel()
	}
	charactersChange   = characters => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {characters})})
	descriptionChange  = e => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {description: e.target.value})})
	edgesChange        = edges => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {edges})})
	gearChange         = gear => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {gear})})
	hindrancesChange   = hindrances => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {hindrances})})
	nameChange         = e => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {name: e.target.value})})
	powersChange       = powers => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {powers})})
	racesChange        = races => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {races})})
	save               = e => {
		e.preventDefault()
		this.props.onSave(this.state.plotPoint)
	}
	settingRulesChange = settingRules => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {settingRules})})
	skillsChange       = skills => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {skills})})
	sectionChange      = section => this.setState({section})
	showSection        = (componentId) => {
		switch (this.state.section) {
			case 'BasicRules':
				return (
					<BasicRules basicRules={this.state.plotPoint.basicRules} onChange={this.basicRulesChange} id={componentId} />)
			case 'SettingRules':
				return (
					<SettingRulesList rules={this.state.plotPoint.settingRules} onChange={this.settingRulesChange}
						id={componentId} />)
			case 'Skills':
				return (
					<Skills skills={this.state.plotPoint.skills} onChange={this.skillsChange} id={componentId} />)
			case 'Edges':
				return (<Edges edges={this.state.plotPoint.edges} onChange={this.edgesChange} id={componentId} />)
			case 'Hindrances':
				return (
					<Hindrances edges={this.state.plotPoint.hindrances} onChange={this.hindrancesChange} id={componentId} />)
			case 'Gear':
				return (<Gear gear={this.state.plotPoint.gear} onChange={this.gearChange} id={componentId} />)
			case 'Powers':
				return (<Powers powers={this.state.plotPoint.powers} onChange={this.powersChange} id={componentId} />)
			case 'Races':
				return (<Races races={this.state.plotPoint.races} onChange={this.racesChange} id={componentId} />)
			case 'Beasts':
				return (<Beasts beasts={this.state.plotPoint.beasts} onChange={this.beastsChange} id={componentId} />)
			case 'Characters':
				return (
					<Characters characters={this.state.plotPoint.characters} onChange={this.charactersChange} id={componentId} />)
		}
	}


	render () {
		const {id}        = this.props
		const {plotPoint} = this.state
		const componentId = `PlotPoint-${id}`
		let errors        = ''
		if (this.props.errors && this.props.errors.length > 0) {
			errors = this.props.errors.map((error, idx) => {
				const errorMessage = typeof error === 'string' ? error : (error.message || 'An error occurred')
				return <Alert key={idx} id={`${id}-error-${idx}`} context={'danger'} >{errorMessage}</Alert >
			})
		}
		return (
			<form id={componentId} >
				{errors}
				<TextFormGroup id={`${componentId}-Name`} label='Name' onChange={this.nameChange} required={true}
					value={plotPoint.name} />

				<TextAreaFormGroup id={`${componentId}-Description`}
					label={'Description'}
					onChange={this.descriptionChange}
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
						<Button id={'save-' + componentId} onClick={this.save} disabled={this.props.disabled} >
							<FontAwesomeIcon icon={'save'} />&nbsp;Save
						</Button >
						<Button id={'cancel-' + componentId} onClick={this.cancel} disabled={this.props.disabled} >
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
		section  : 'BasicRules',
		plotPoint: this.props.plotPoint
	}

}


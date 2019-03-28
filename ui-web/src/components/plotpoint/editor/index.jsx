import {FontAwesomeIcon}       from '@fortawesome/react-fontawesome'
import {Button, TextFormGroup} from 'bootstrap-react-components'
import {func, string}          from 'prop-types'
import React                   from 'react'
import RichTextEditor          from 'react-rte'

import '../../../../node_modules/draft-js/dist/Draft.css'
import PlotPointPropType       from '../../../propTypes/PlotPoint'
import BasicRules              from './BasicRules'
import Beasts                  from './beasts'
import Characters              from './characters'
import Edges                   from './edges'
import Gear                    from './gear'
import Hindrances              from './hindrances'
import Navigation              from './Navigation'
import Powers                  from './powers'
import Races                   from './races'
import SettingRulesList        from './setting_rules'
import Skills                  from './skills'

export default class PlotPointForm extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id       : string.isRequired,
		onCancel : func.isRequired,
		onSave   : func.isRequired,
		plotPoint: PlotPointPropType.isRequired
	}

	basicRulesChange   = basicRules => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {basicRules})})
	beastsChange       = beasts => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {beasts})})
	charactersChange   = characters => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {characters})})
	descriptionChange  = editorState => this.setState({editorState})
	edgesChange        = edges => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {edges})})
	gearChange         = gear => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {gear})})
	hindrancesChange   = hindrances => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {hindrances})})
	nameChange         = e => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {name: e.target.value})})
	powersChange       = powers => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {powers})})
	racesChange        = races => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {races})})
	settingRulesChange = settingRules => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {settingRules})})
	skillsChange       = skills => this.setState({plotPoint: Object.assign({}, this.state.plotPoint, {skills})})
	sectionChange      = section => this.setState({section})
	showSection        = (componentId) => {
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
		const {id}        = this.props
		const {plotPoint} = this.state
		const componentId = `PlotPoint-${id}`
		return (
			<form id={componentId} >
				<div className={'row'} >
					<div className={'col-12'} >
						<TextFormGroup id={`${componentId}-Name`} label='Name' onChange={this.nameChange} required={true}
							value={plotPoint.name} />
					</div >
				</div >
				<div className={'row'} >
					<div className={'col-12'} >
						<label >Description:</label >
						<RichTextEditor value={this.state.editorState} onChange={this.descriptionChange} />
					</div >
				</div >
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
		section    : 'BasicRules',
		editorState: RichTextEditor.createEmptyValue(),
		plotPoint  : this.props.plotPoint
	}

}


import {API} from 'aws-amplify'
import {Button, NumberFormGroup, PageHeader, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components'
import React from 'react'
import {withRouter} from 'react-router'
import AircraftEditorList from '../components/lists/AircraftEditorList'
import AmmunitionEditorList from '../components/lists/AmmunitionEditorList'
import ArcaneBackgroundEditorList from '../components/lists/ArcaneBackgroundEditorList'
import ArmorEditorList from '../components/lists/ArmorEditorList'
import BeastsEditorList from '../components/lists/BeastsEditorList'
import CharacterEditorList from '../components/lists/CharacterEditorList'
import EdgeEditorList from '../components/lists/EdgeEditorList'
import GroundVehiclesEditorList from '../components/lists/GroundVehiclesEditorList'
import HandWeaponsEditorList from '../components/lists/HandWeaponsEditorList'
import HindranceEditorList from '../components/lists/HindranceEditorList'
import MundaneItemEditorList from '../components/lists/MundaneItemEditorList'
import PowersEditorList from '../components/lists/PowersEditorList'
import RaceEditorList from '../components/lists/RaceEditorList'
import RangedWeaponEditorList from '../components/lists/RangedWeaponEditorList'
import SettingRulesList from '../components/lists/SettingRules'
import SkillEditorList from '../components/lists/SkillEditorList'
import SpecialWeaponsEditorList from '../components/lists/SpecialWeaponsEditorList'
import TrappingsAndEffectsEditorList from '../components/lists/TrappingsAndEffectsEditorList'
import VehicleMountedAndAtGunsEditorList from '../components/lists/VehicleMountedAndAtGunsEditorList'
import WatercraftEditorList from '../components/lists/WatercraftEditorList'
import './PlotPointEditor.css'


class PlotPointEditor extends React.Component {

	static defaultProps = {
		id: 'PlotPointEditorPage'
	}

	state = {
		aircraft               : [],
		ammunition             : [],
		arcaneBackgrounds      : [],
		armor                  : [],
		beasts                 : [],
		characters             : [],
		description            : ' ',
		edges                  : [],
		groundVehicles         : [],
		handWeapons            : [],
		hindrances             : [],
		maximumAttributePoints : 5,
		maximumMajorHindrances : 1,
		maximumMinorHindrances : 2,
		maximumSkillPoints     : 15,
		mundaneItems           : [],
		name                   : ' ',
		powers                 : [],
		races                  : [],
		rangedWeapons          : [],
		settingRules           : [],
		skills                 : [],
		specialWeapons         : [],
		trappingsAndEffects    : [],
		vehicleMountedAndAtGuns: [],
		watercraft             : []
	}

	aircraftChange                = aircraft => this.setState({aircraft})
	ammunitionChange              = ammunition => this.setState({ammunition})
	arcaneBackgroundChange        = arcaneBackgrounds => this.setState({arcaneBackgrounds})
	armorChange                   = armor => this.setState({armor})
	beastsChange                  = beasts => this.setState({beasts})
	charactersChange              = characters => this.setState({characters})
	cancel                        = e => {
		e.preventDefault()
		this.props.cancel()
	}
	descriptionChange             = e => this.setState({description: e.target.value})
	edgesChange                   = edges => this.setState({edges})
	groundVehiclesChange          = groundVehicles => this.setState({groundVehicles})
	handWeaponsChange             = handWeapons => this.setState({handWeapons})
	hindrancesChange              = hindrances => this.setState({hindrances})
	maximumAttributePointsChange  = e => this.setState({maximumAttributePoints: parseInt(e.target.value, 10)})
	maximumMajorHindrancesChange  = e => this.setState({maximumMajorHindrances: parseInt(e.target.value, 10)})
	maximumMinorHindrancesChange  = e => this.setState({maximumMinorHindrances: parseInt(e.target.value, 10)})
	maximumSkillPointsChange      = e => this.setState({maximumSkillPoints: parseInt(e.target.value, 10)})
	mundaneItemsChange            = mundaneItems => this.setState({mundaneItems})
	nameChange                    = e => this.setState({name: e.target.value})
	powersChange                  = powers => this.setState({powers})
	racesChange                   = races => this.setState({races})
	rangedWeaponsChange           = rangedWeapons => this.setState({rangedWeapons})
	settingRulesChange            = settingRules => this.setState({settingRules: settingRules})
	skillsChange                  = skills => this.setState({skills})
	specialWeaponsChange          = specialWeapons => this.setState({specialWeapons})
	trappingsAndEffectsChange     = trappingsAndEffects => this.setState({trappingsAndEffects})
	vehicleMountedAndAtGunsChange = vehicleMountedAndAtGuns => this.setState({vehicleMountedAndAtGuns})
	watercraftChange              = watercraft => this.setState({watercraft})

	save = async e => {
		e.preventDefault()
		let toSave = {
			aircraft               : this.state.aircraft,
			ammunition             : this.state.ammunition,
			arcaneBackgrounds      : this.state.arcaneBackgrounds,
			armor                  : this.state.armor,
			beasts                 : this.state.beasts,
			characters             : this.state.characters,
			description            : this.state.description,
			edges                  : this.state.edges,
			groundVehicles         : this.state.groundVehicles,
			handWeapons            : this.state.handWeapons,
			hindrances             : this.state.hindrances,
			maximumAttributePoints : this.state.maximumAttributePoints,
			maximumMajorHindrances : this.state.maximumMajorHindrances,
			maximumMinorHindrances : this.state.maximumMinorHindrances,
			maximumSkillPoints     : this.state.maximumSkillPoints,
			mundaneItems           : this.state.mundaneItems,
			name                   : this.state.name,
			powers                 : this.state.powers,
			races                  : this.state.races,
			rangedWeapons          : this.state.rangedWeapons,
			settingRules           : this.state.settingRules,
			skills                 : this.state.skills,
			specialWeapons         : this.state.specialWeapons,
			trappingsAndEffects    : this.state.trappingsAndEffects,
			vehicleMountedAndAtGuns: this.state.vehicleMountedAndAtGuns,
			watercraft             : this.state.watercraft
		}
		if (this.props.match.params.name) {
			await API.put('PlotPointsCRUD', `/PlotPoints`, {
				body: {...toSave}
			})
		} else {
			await API.post('PlotPointsCRUD', `/PlotPoints`, {
				body: {...toSave}
			})
		}

		this.props.history.push('/')
	}

	async componentDidMount() {
		if (this.props.match.params.name) {
			let plotPoint = await API.get('PlotPointsCRUD', `/PlotPoints/object/${this.props.match.params.name}`, {})
			this.setState({
				...plotPoint
			})
		}
	};

	render() {
		let componentId = `PlotPointEditor-${this.props.id}`
		return <div id={componentId} className='container-fluid'>
			<PageHeader id={componentId}><h1>Plot Point Editor</h1></PageHeader>
			<div class="row">
				<nav className="col-md-2 d-none d-md-block bg-light sidebar">
					<div className="sidebar-sticky">
						<ul className="nav flex-column">
							<li className="nav-item">
								<a className="nav-link active" href="#">
									<span data-feather="home"></span>
									Dashboard <span className="sr-only">(current)</span>
								</a>
							</li>
							<li className="nav-item">
								<a className="nav-link" href="#">
									<span data-feather="file"></span>
									Orders
								</a>
							</li>
							<li className="nav-item">
								<a className="nav-link" href="#">
									<span data-feather="shopping-cart"></span>
									Products
								</a>
							</li>
							<li className="nav-item">
								<a className="nav-link" href="#">
									<span data-feather="users"></span>
									Customers
								</a>
							</li>
							<li className="nav-item">
								<a className="nav-link" href="#">
									<span data-feather="bar-chart-2"></span>
									Reports
								</a>
							</li>
							<li className="nav-item">
								<a className="nav-link" href="#">
									<span data-feather="layers"></span>
									Integrations
								</a>
							</li>
						</ul>

						<h6 className="sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted">
							<span>Saved reports</span>
							<a className="d-flex align-items-center text-muted" href="#">
								<span data-feather="plus-circle"></span>
							</a>
						</h6>
						<ul className="nav flex-column mb-2">
							<li className="nav-item">
								<a className="nav-link" href="#">
									<span data-feather="file-text"></span>
									Current month
								</a>
							</li>
							<li className="nav-item">
								<a className="nav-link" href="#">
									<span data-feather="file-text"></span>
									Last quarter
								</a>
							</li>
							<li className="nav-item">
								<a className="nav-link" href="#">
									<span data-feather="file-text"></span>
									Social engagement
								</a>
							</li>
							<li className="nav-item">
								<a className="nav-link" href="#">
									<span data-feather="file-text"></span>
									Year-end sale
								</a>
							</li>
						</ul>
					</div>
				</nav>
				<div class="col-md-9 ml-sm-auto col-lg-10 pt-3 px-4">
					<form id={`Form-${componentId}`}>
						<TextFormGroup id={`${componentId}-Name`} label='Name' onChange={this.nameChange} required={true}
						               value={this.state.name}/>
						<TextAreaFormGroup id={`${componentId}-Description`} label={'Description'} onChange={this.descriptionChange}
						                   value={this.state.description}/>
						<h1>Basic Rules</h1>
						<NumberFormGroup id={`${componentId}-MaximumAttributePoints`} label={'Maximum Attribute Points'}
						                 onChange={this.maximumAttributePointsChange} required={true}
						                 value={this.state.maximumAttributePoints}/>
						<NumberFormGroup id={`${componentId}-MaximumMajorHindrances`} label={'Maximum Number of Major Hindrances'}
						                 onChange={this.maximumMajorHindrancesChange} required={true}
						                 value={this.state.maximumMajorHindrances}/>
						<NumberFormGroup id={`${componentId}-MaximumMinorHindrances`} label={'Maximum Number of Minor Hindrances'}
						                 onChange={this.maximumMinorHindrancesChange} required={true}
						                 value={this.state.maximumMinorHindrances}/>
						<NumberFormGroup id={`${componentId}-MaximumSkillPoints`} label={'Maximum Skill Points'}
						                 onChange={this.maximumSkillPointsChange} required={true}
						                 value={this.state.maximumSkillPoints}/>
						<h1>Setting Rules</h1>
						<SettingRulesList id={`${componentId}`} onChange={this.settingRulesChange} rules={this.state.settingRules}/>
						<h1>Character Creation</h1>
						<RaceEditorList id={componentId} races={this.state.races} racesChange={this.racesChange}/>
						<SkillEditorList id={componentId} skills={this.state.skills} skillsChange={this.skillsChange}/>
						<HindranceEditorList id={componentId} hindrances={this.state.hindrances}
						                     hindrancesChange={this.hindrancesChange}/>
						<EdgeEditorList id={componentId} edges={this.state.edges} edgesChange={this.edgesChange}/>
						<h1>Gear</h1>
						<MundaneItemEditorList id={componentId} mundaneItems={this.state.mundaneItems}
						                       mundaneItemsChange={this.mundaneItemsChange}/>
						<HandWeaponsEditorList id={componentId} handWeapons={this.state.handWeapons}
						                       handWeaponsChange={this.handWeaponsChange}/>
						<ArmorEditorList id={componentId} armor={this.state.armor} armorChange={this.armorChange}/>
						<RangedWeaponEditorList id={componentId} rangedWeapons={this.state.rangedWeapons}
						                        rangedWeaponsChange={this.rangedWeaponsChange}/>
						<VehicleMountedAndAtGunsEditorList id={componentId}
						                                   vehicleMountedAndAtGuns={this.state.vehicleMountedAndAtGuns}
						                                   vehicleMountedAndAtGunsChange={this.vehicleMountedAndAtGunsChange}/>
						<AmmunitionEditorList id={componentId} ammunition={this.state.ammunition}
						                      ammunitionChange={this.ammunitionChange}/>
						<SpecialWeaponsEditorList id={componentId} specialWeapons={this.state.specialWeapons}
						                          specialWeaponsChange={this.specialWeaponsChange}/>
						<h1>Vehicles</h1>
						<GroundVehiclesEditorList id={componentId} groundVehicles={this.state.groundVehicles}
						                          groundVehiclesChange={this.groundVehiclesChange}/>
						<WatercraftEditorList id={componentId} watercraft={this.state.watercraft}
						                      watercraftChange={this.watercraftChange}/>
						<AircraftEditorList id={componentId} aircraft={this.state.aircraft} aircraftChange={this.aircraftChange}/>
						<h1>Powers</h1>
						<ArcaneBackgroundEditorList id={componentId} arcaneBackgrounds={this.state.arcaneBackgrounds}
						                            arcaneBackgroundChange={this.arcaneBackgroundChange}/>
						<TrappingsAndEffectsEditorList id={componentId} trappingsAndEffects={this.state.trappingsAndEffects}
						                               trappingsAndEffectsChange={this.trappingsAndEffectsChange}/>
						<PowersEditorList id={componentId} powers={this.state.powers} powersChange={this.powersChange}/>
						<BeastsEditorList id={componentId} beasts={this.state.beasts} beastsChange={this.beastsChange}
						                  skills={this.state.skills}/>
						<h1>Characters</h1>
						<CharacterEditorList id={componentId} characters={this.state.characters}
						                     charactersChange={this.charactersChange}/>
						<Button id={componentId} onClick={this.save}>Save</Button>
						<Button id={componentId} onClick={this.cancel}>Cancel</Button>
					</form>
				</div>
			</div>
		</div>
	}
}

export default withRouter(PlotPointEditor)

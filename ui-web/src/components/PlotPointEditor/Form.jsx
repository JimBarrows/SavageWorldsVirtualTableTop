import {Button} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import Aircraft from './Aircraft'
import AmmunitionEditorList from '../lists/AmmunitionEditorList'
import ArcaneBackgroundEditorList from '../lists/ArcaneBackgroundEditorList'
import Armor from './Armor'
import BeastsEditorList from '../lists/BeastsEditorList'
import CharacterEditorList from '../lists/CharacterEditorList'
import Edges from './Edges'
import GroundVehiclesEditorList from '../lists/GroundVehiclesEditorList'
import HandWeapons from './HandWeapons'
import Hindrances from './Hindrances'
import MundaneItems from './MundaneItems'
import PowersEditorList from '../lists/PowersEditorList'
import Races from './Races'
import RangedWeapons from './RangedWeapons'
import Skills from './Skills'
import SettingRules from './SettingRules'
import SpecialWeaponsEditorList from '../lists/SpecialWeaponsEditorList'
import TrappingsAndEffectsEditorList from '../lists/TrappingsAndEffectsEditorList'
import VehicleMountedAndAtGunsEditorList from '../lists/VehicleMountedAndAtGunsEditorList'
import WatercraftEditorList from '../lists/WatercraftEditorList'
import PlotPoint from './PlotPoint'

export default class Form extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id       : PropTypes.string.isRequired,
		plotPoint: PropTypes.object.isRequired,
		save     : PropTypes.func.isRequired,
		show     : PropTypes.oneOf(['Aircraft','Armor', 'Edges', 'HandWeapons', 'Hindrances', 'MundaneItems', 'Races', 'RangedWeapons', 'PlotPoint', 'SettingRules', 'Skills']).isRequired
	}


	aircraftChange                = aircraft => this.props.onChange(Object.assign({}, this.props.plotPoint, {aircraft}))
	ammunitionChange              = ammunition => this.props.onChange(Object.assign({}, this.props.plotPoint, {ammunition}))
	arcaneBackgroundChange        = arcaneBackgrounds => this.props.onChange(Object.assign({}, this.props.plotPoint, {arcaneBackgrounds}))
	armorChange                   = armor => this.props.onChange(Object.assign({}, this.props.plotPoint, {armor}))
	beastsChange                  = beasts => this.props.onChange(Object.assign({}, this.props.plotPoint, {beasts}))
	charactersChange              = characters => this.props.onChange(Object.assign({}, this.props.plotPoint, {characters}))
	edgesChange                   = edges => this.props.onChange(Object.assign({}, this.props.plotPoint, {edges}))
	groundVehiclesChange          = groundVehicles => this.props.onChange(Object.assign({}, this.props.plotPoint, {groundVehicles}))
	handWeaponsChange             = handWeapons => this.props.onChange(Object.assign({}, this.props.plotPoint, {handWeapons}))
	hindrancesChange              = hindrances => this.props.onChange(Object.assign({}, this.props.plotPoint, {hindrances}))
	mundaneItemsChange            = mundaneItems => this.props.onChange(Object.assign({}, this.props.plotPoint, {mundaneItems}))
	nameChange                    = e => this.props.onChange(Object.assign({}, this.props.plotPoint, {name: e.target.value}))
	powersChange                  = powers => this.props.onChange(Object.assign({}, this.props.plotPoint, {powers}))
	rangedWeaponsChange           = rangedWeapons => this.props.onChange(Object.assign({}, this.props.plotPoint, {rangedWeapons}))
	skillsChange                  = skills => this.props.onChange(Object.assign({}, this.props.plotPoint, {skills}))
	specialWeaponsChange          = specialWeapons => this.props.onChange(Object.assign({}, this.props.plotPoint, {specialWeapons}))
	trappingsAndEffectsChange     = trappingsAndEffects => this.props.onChange(Object.assign({}, this.props.plotPoint, {trappingsAndEffects}))
	vehicleMountedAndAtGunsChange = vehicleMountedAndAtGuns => this.props.onChange(Object.assign({}, this.props.plotPoint, {vehicleMountedAndAtGuns}))
	watercraftChange              = watercraft => this.props.onChange(Object.assign({}, this.props.plotPoint, {watercraft}))

	cancel   = e => {
		e.preventDefault()
		this.props.cancel()
	}
	onChange = plotPoint => this.props.onChange(Object.assign({}, this.props.plotPoint, plotPoint))
	save     = e => {
		e.preventDefault()
		this.props.save(this.props.plotPoint)
	}


	render() {
		let {id, plotPoint, show} = this.props
		let component_id           = `Form-${id}`
		let component             = <div><h1>No Components</h1></div>
		switch (show) {
			case 'Aircraft': component = 	<Aircraft id={component_id} aircraft={plotPoint.aircraft} aircraftChange={this.aircraftChange}/>
									break
			case 'Armor': component = <Armor id={component_id} armor={plotPoint.armor} armorChange={this.armorChange}/>
									break
			case 'Edges': component = <Edges id={component_id} edges={plotPoint.edges} edgesChange={this.edgesChange}/>
									break
			case 'HandWeapons': component = <HandWeapons id={component_id} handWeapons={plotPoint.handWeapons}
														 handWeaponsChange={this.handWeaponsChange}/>
													 break
			case 'Hindrances' : component = <Hindrances id={component_id} hindrances={plotPoint.hindrances}
													 hindrancesChange={this.hindrancesChange}/>
												 break
			case 'MundaneItems': component = <MundaneItems id={component_id} mundaneItems={plotPoint.mundaneItems}
														 mundaneItemsChange={this.mundaneItemsChange}/>
													 break
			case 'RangedWeapons': component = <RangedWeapons id={component_id} rangedWeapons={plotPoint.rangedWeapons}
															rangedWeaponsChange={this.rangedWeaponsChange}/>
														break
			case 'SettingRules' :
				component =	<SettingRules id={`${component_id}`} onChange={this.settingRulesChange} rules={plotPoint.settingRules}/>
				break
			case 'Skills' :
					component = <Skills id={component_id} skills={plotPoint.skills} skillsChange={this.skillsChange}/>
					break
			case 'Races' :
				component = <Races id={component_id} races={plotPoint.races} onChange={this.onChange}/>
				break
			case 'PlotPoint':
			default:
				component = <PlotPoint id={component_id} plotPoint={plotPoint} onChange={this.onChange}/>
				break
		}
		return <div className="col-md-9 ml-sm-auto col-lg-10 pt-3 px-4">
			<form id={`${component_id}`}>
				{component}
				<VehicleMountedAndAtGunsEditorList id={component_id}
				                                   vehicleMountedAndAtGuns={plotPoint.vehicleMountedAndAtGuns}
				                                   vehicleMountedAndAtGunsChange={this.vehicleMountedAndAtGunsChange}/>
				<AmmunitionEditorList id={component_id} ammunition={plotPoint.ammunition}
				                      ammunitionChange={this.ammunitionChange}/>
				<SpecialWeaponsEditorList id={component_id} specialWeapons={plotPoint.specialWeapons}
				                          specialWeaponsChange={this.specialWeaponsChange}/>
				<h1>Vehicles</h1>
				<GroundVehiclesEditorList id={component_id} groundVehicles={plotPoint.groundVehicles}
				                          groundVehiclesChange={this.groundVehiclesChange}/>
				<WatercraftEditorList id={component_id} watercraft={plotPoint.watercraft}
				                      watercraftChange={this.watercraftChange}/>

				<h1>Powers</h1>
				<ArcaneBackgroundEditorList id={component_id} arcaneBackgrounds={plotPoint.arcaneBackgrounds}
				                            arcaneBackgroundChange={this.arcaneBackgroundChange}/>
				<TrappingsAndEffectsEditorList id={component_id} trappingsAndEffects={plotPoint.trappingsAndEffects}
				                               trappingsAndEffectsChange={this.trappingsAndEffectsChange}/>
				<PowersEditorList id={component_id} powers={plotPoint.powers} powersChange={this.powersChange}/>
				<BeastsEditorList id={component_id} beasts={plotPoint.beasts} beastsChange={this.beastsChange}
				                  skills={plotPoint.skills}/>
				<h1>Characters</h1>
				<CharacterEditorList id={component_id} characters={plotPoint.characters}
				                     charactersChange={this.charactersChange}/>
				<Button id={component_id} onClick={this.save}>Save</Button>
				<Button id={component_id} onClick={this.cancel}>Cancel</Button>
			</form>
		</div>
	}
}

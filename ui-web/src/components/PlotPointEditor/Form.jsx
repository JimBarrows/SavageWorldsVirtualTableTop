import {Button} from 'bootstrap-react-components'
import PropTypes from 'prop-types'
import React from 'react'
import AircraftEditorList from '../lists/AircraftEditorList'
import AmmunitionEditorList from '../lists/AmmunitionEditorList'
import ArcaneBackgroundEditorList from '../lists/ArcaneBackgroundEditorList'
import ArmorEditorList from '../lists/ArmorEditorList'
import BeastsEditorList from '../lists/BeastsEditorList'
import CharacterEditorList from '../lists/CharacterEditorList'
import EdgeEditorList from '../lists/EdgeEditorList'
import GroundVehiclesEditorList from '../lists/GroundVehiclesEditorList'
import HandWeaponsEditorList from '../lists/HandWeaponsEditorList'
import HindranceEditorList from '../lists/HindranceEditorList'
import MundaneItemEditorList from '../lists/MundaneItemEditorList'
import PowersEditorList from '../lists/PowersEditorList'
import RaceEditorList from '../lists/RaceEditorList'
import Race from './Race'
import RangedWeaponEditorList from '../lists/RangedWeaponEditorList'
import SkillEditorList from '../lists/SkillEditorList'
import SpecialWeaponsEditorList from '../lists/SpecialWeaponsEditorList'
import TrappingsAndEffectsEditorList from '../lists/TrappingsAndEffectsEditorList'
import VehicleMountedAndAtGunsEditorList from '../lists/VehicleMountedAndAtGunsEditorList'
import WatercraftEditorList from '../lists/WatercraftEditorList'
import PlotPoint from './PlotPoint'
import SettingRules from './SettingRules'

export default class Form extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id       : PropTypes.string.isRequired,
		plotPoint: PropTypes.object.isRequired,
		save     : PropTypes.func.isRequired,
		show     : PropTypes.oneOf(['PlotPoint', 'SettingRules']).isRequired
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
		let componentId           = `Form-${id}`
		let component             = <div><h1>No Components</h1></div>
		switch (show) {
			case 'SettingRules' :
				component = <SettingRules id={componentId} plotPoint={plotPoint} onChange={this.onChange}/>
				break
			case 'Race' :
				component = <Race id={componentId} plotPoint={plotPoint} onChange={this.onChange}/>
				break
			case 'PlotPoint':
			default:
				component = <PlotPoint id={componentId} plotPoint={plotPoint} onChange={this.onChange}/>
				break
		}
		return <div className="col-md-9 ml-sm-auto col-lg-10 pt-3 px-4">
			<form id={`${componentId}`}>
				{component}
				<h1>Character Creation</h1>

				<SkillEditorList id={componentId} skills={plotPoint.skills} skillsChange={this.skillsChange}/>
				<HindranceEditorList id={componentId} hindrances={plotPoint.hindrances}
				                     hindrancesChange={this.hindrancesChange}/>
				<EdgeEditorList id={componentId} edges={plotPoint.edges} edgesChange={this.edgesChange}/>
				<h1>Gear</h1>
				<MundaneItemEditorList id={componentId} mundaneItems={plotPoint.mundaneItems}
				                       mundaneItemsChange={this.mundaneItemsChange}/>
				<HandWeaponsEditorList id={componentId} handWeapons={plotPoint.handWeapons}
				                       handWeaponsChange={this.handWeaponsChange}/>
				<ArmorEditorList id={componentId} armor={plotPoint.armor} armorChange={this.armorChange}/>
				<RangedWeaponEditorList id={componentId} rangedWeapons={plotPoint.rangedWeapons}
				                        rangedWeaponsChange={this.rangedWeaponsChange}/>
				<VehicleMountedAndAtGunsEditorList id={componentId}
				                                   vehicleMountedAndAtGuns={plotPoint.vehicleMountedAndAtGuns}
				                                   vehicleMountedAndAtGunsChange={this.vehicleMountedAndAtGunsChange}/>
				<AmmunitionEditorList id={componentId} ammunition={plotPoint.ammunition}
				                      ammunitionChange={this.ammunitionChange}/>
				<SpecialWeaponsEditorList id={componentId} specialWeapons={plotPoint.specialWeapons}
				                          specialWeaponsChange={this.specialWeaponsChange}/>
				<h1>Vehicles</h1>
				<GroundVehiclesEditorList id={componentId} groundVehicles={plotPoint.groundVehicles}
				                          groundVehiclesChange={this.groundVehiclesChange}/>
				<WatercraftEditorList id={componentId} watercraft={plotPoint.watercraft}
				                      watercraftChange={this.watercraftChange}/>
				<AircraftEditorList id={componentId} aircraft={plotPoint.aircraft} aircraftChange={this.aircraftChange}/>
				<h1>Powers</h1>
				<ArcaneBackgroundEditorList id={componentId} arcaneBackgrounds={plotPoint.arcaneBackgrounds}
				                            arcaneBackgroundChange={this.arcaneBackgroundChange}/>
				<TrappingsAndEffectsEditorList id={componentId} trappingsAndEffects={plotPoint.trappingsAndEffects}
				                               trappingsAndEffectsChange={this.trappingsAndEffectsChange}/>
				<PowersEditorList id={componentId} powers={plotPoint.powers} powersChange={this.powersChange}/>
				<BeastsEditorList id={componentId} beasts={plotPoint.beasts} beastsChange={this.beastsChange}
				                  skills={plotPoint.skills}/>
				<h1>Characters</h1>
				<CharacterEditorList id={componentId} characters={plotPoint.characters}
				                     charactersChange={this.charactersChange}/>
				<Button id={componentId} onClick={this.save}>Save</Button>
				<Button id={componentId} onClick={this.cancel}>Cancel</Button>
			</form>
		</div>
	}
}

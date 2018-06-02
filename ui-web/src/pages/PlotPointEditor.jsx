import {API} from 'aws-amplify';
import {PageHeader} from 'bootstrap-react-components';
import React from 'react';
import {withRouter} from 'react-router';
import AircraftEditor from '../components/AircraftEditor';
import AmmunitionEditor from '../components/AmmunitionEditor';
import ArcaneBackgroundEditor from '../components/ArcaneBackgroundEditor';
import ArmorEditor from '../components/ArmorEditor';
import BaseVehicleEditor from '../components/BaseVehicleEditor';
import BeastEditor from '../components/BeastEditor';
import EdgeEditor from '../components/EdgeEditor';
import EditorList from '../components/EditorList';
import HandWeaponEditor from '../components/HandWeaponEditor';
import HindranceEditorList from '../components/HindranceEditorList';
import MundaneItemEditor from '../components/MundaneItemEditor';
import NumberFormGroup from '../components/NumberFormGroup';
import PowerEditor from '../components/PowerEditor';
import RaceEditorList from '../components/RaceEditorList';
import RangedWeaponEditor from '../components/RangedWeaponEditor';
import SkillEditorList from '../components/SkillEditorList';
import SpecialWeaponsEditor from '../components/SpecialWeaponsEditor';
import TextAreaFormGroup from '../components/TextAreaFormGroup';
import TextFormGroup from '../components/TextFormGroup';
import TrappingsAndEffectsEditor from '../components/TrappingsAndEffectsEditor';
import VehicleMountedAndAtGunsEditor from '../components/VehicleMountedAndAtGunsEditor';
import WatercraftEditor from '../components/WatercraftEditor';


class PlotPointEditor extends React.Component {

	static defaultProps = {
		id: 'PlotPointEditorPage'
	};

	state = {
		aircraft               : [],
		ammunition             : [],
		arcaneBackgrounds      : [],
		armor                  : [],
		beasts                 : [],
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
		skills                 : [],
		specialWeapons         : [],
		trappingsAndEffects    : [],
		vehicleMountedAndAtGuns: [],
		watercraft             : []
	};

	aircraftChange                = aircraft => this.setState({aircraft});
	ammunitionChange              = ammunition => this.setState({ammunition});
	arcaneBackgroundChange        = arcaneBackgrounds => this.setState({arcaneBackgrounds});
	armorChange                   = armor => this.setState({armor});
	beastsChange                  = beasts => this.setState({beasts});
	cancel                        = e => {
		e.preventDefault();
		this.props.cancel();
	};
	descriptionChange             = e => this.setState({description: e.target.value});
	edgeListChange                = edges => this.setState({edges});
	groundVehiclesChange          = groundVehicles => this.setState({groundVehicles});
	handWeaponsChange             = handWeapons => this.setState({handWeapons});
	hindrancesChange              = hindrances => this.setState({hindrances});
	maximumAttributePointsChange  = e => this.setState({maximumAttributePoints: parseInt(e.target.value, 10)});
	maximumMajorHindrancesChange  = e => this.setState({maximumMajorHindrances: parseInt(e.target.value, 10)});
	maximumMinorHindrancesChange  = e => this.setState({maximumMinorHindrances: parseInt(e.target.value, 10)});
	maximumSkillPointsChange      = e => this.setState({maximumSkillPoints: parseInt(e.target.value, 10)});
	mundaneItemsChange            = mundaneItems => this.setState({mundaneItems});
	nameChange                    = e => this.setState({name: e.target.value});
	powersChange                  = powers => this.setState({powers});
	racesChange                   = races => this.setState({races});
	rangedWeaponsChange           = rangedWeapons => this.setState({rangedWeapons});
	skillsChange                  = skills => this.setState({skills});
	specialWeaponsChange          = specialWeapons => this.setState({specialWeapons});
	trappingsAndEffectsChange     = trappingsAndEffects => this.setState({trappingsAndEffects});
	vehicleMountedAndAtGunsChange = vehicleMountedAndAtGuns => this.setState({vehicleMountedAndAtGuns});
	watercraftChange              = watercraft => this.setState({watercraft});

	save = async e => {
		e.preventDefault();
		let toSave = {
			aircraft               : this.state.aircraft,
			ammunition             : this.state.ammunition,
			arcaneBackgrounds      : this.state.arcaneBackgrounds,
			armor                  : this.state.armor,
			beasts                 : this.state.beasts,
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
			skills                 : this.state.skills,
			specialWeapons         : this.state.specialWeapons,
			trappingsAndEffects    : this.state.trappingsAndEffects,
			vehicleMountedAndAtGuns: this.state.vehicleMountedAndAtGuns,
			watercraft             : this.state.watercraft
		};
		if (this.props.match.params.name) {
			await API.put('PlotPointsCRUD', `/PlotPoints`, {
				body: {...toSave}
			});
		} else {
			await API.post('PlotPointsCRUD', `/PlotPoints`, {
				body: {...toSave}
			});
		}

		this.props.history.push('/');
	};

	async componentDidMount() {
		if (this.props.match.params.name) {
			let plotPoint = await API.get('PlotPointsCRUD', `/PlotPoints/object/${this.props.match.params.name}`);
			this.setState({
				...plotPoint
			});
		}
	};

	render() {
		return <div id={this.props.id}>
			<PageHeader id={this.props.id}><h1>Plot Point Editor</h1></PageHeader>
			<form id='plotPointForm'>
				<h1>Basic Rules</h1>
				<TextFormGroup id='plotPointName' label='Name' onChange={this.nameChange} required={true}
				               value={this.state.name}/>
				<TextAreaFormGroup id={'plotPointDescription'} label={'Description'} onChange={this.descriptionChange}
				                   value={this.state.description}/>
				<NumberFormGroup id={'maximumAttributePoints'} label={'Maximum Attribute Points'}
				                 onChange={this.maximumAttributePointsChange} required={true}
				                 value={this.state.maximumAttributePoints}/>
				<NumberFormGroup id={'maximumMajorHindrances'} label={'Maximum Number of Major Hindrances'}
				                 onChange={this.maximumMajorHindrancesChange} required={true}
				                 value={this.state.maximumMajorHindrances}/>
				<NumberFormGroup id={'maximumMinorHindrances'} label={'Maximum Number of Minor Hindrances'}
				                 onChange={this.maximumMinorHindrancesChange} required={true}
				                 value={this.state.maximumMinorHindrances}/>
				<NumberFormGroup id={'maximumSkillPoints'} label={'Maximum Skill Points'}
				                 onChange={this.maximumSkillPointsChange} required={true}
				                 value={this.state.maximumSkillPoints}/>
				<h1>Character Creation</h1>
				<RaceEditorList id={'PlotPoint'} races={this.state.races} racesChange={this.racesChange}/>
				<SkillEditorList id={'PlotPoint'} skills={this.state.skills} skillsChange={this.skillsChange}/>
				<HindranceEditorList id={'PlotPoint'} hindrances={this.state.hindrances}
				                     hindrancesChange={this.hindrancesChange}/>
				<EditorList emptyItem={({name: ' ', description: ' ', category: ' '})}
				            id={'EdgeEditorList'}
				            list={this.state.edges}
				            onChange={this.edgeListChange}
				            title={'Edges'}>
					<EdgeEditor/>
				</EditorList>
				<h1>Gear</h1>
				<EditorList emptyItem={({name: ' ', description: ' ', cost: 1, weight: 1})}
				            id={'MundaneItemEditorList'}
				            list={this.state.mundaneItems}
				            onChange={this.mundaneItemsChange}
				            title={'Mundane Items'}>
					<MundaneItemEditor/>
				</EditorList>
				<EditorList
						emptyItem={({
							name       : ' ',
							description: ' ',
							cost       : 1,
							weight     : 1,
							damage     : ' ',
							notes      : ' ',
							era        : ' ',
							kind       : ' '
						})}
						id={'HandWeaponsEditorList'}
						list={this.state.handWeapons}
						onChange={this.handWeaponsChange}
						title={'Hand Weapons'}>
					<HandWeaponEditor/>
				</EditorList>
				<EditorList
						emptyItem={({name: ' ', description: ' ', cost: 1, weight: 1, armor: ' ', notes: ' ', era: ' ', kind: ' '})}
						id={'ArmorEditorList'}
						list={this.state.armor}
						onChange={this.armorChange}
						title={'Armor'}>
					<ArmorEditor/>
				</EditorList>
				<EditorList
						emptyItem={({
							name           : ' ',
							description    : ' ',
							cost           : 1,
							weight         : 1,
							shortRange     : 1,
							mediumRange    : 2,
							longRange      : 3,
							damage         : ' ',
							rateOfFire     : 1,
							shots          : 1,
							minimumStrength: ' ',
							notes          : ' ',
							era            : ' ',
							kind           : ' '
						})}
						id={'RangedWeaponEditorList'}
						list={this.state.rangedWeapons}
						onChange={this.rangedWeaponsChange}
						title={'Ranged Weapons'}>
					<RangedWeaponEditor/>
				</EditorList>
				<EditorList
						emptyItem={({
							name           : ' ',
							description    : ' ',
							cost           : 1,
							weight         : 1,
							shortRange     : 1,
							mediumRange    : 2,
							longRange      : 3,
							apDamage       : ' ',
							apArmorPiercing: 1,
							heDamage       : ' ',
							heBurstTemplate: ' ',
							heArmorPiercing: 1,
							rateOfFire     : 1,
							notes          : ' ',
							era            : ' ',
							kind           : ' '
						})}
						id={'vehicleMountedAndAtGunsEditorList'}
						list={this.state.vehicleMountedAndAtGuns}
						onChange={this.vehicleMountedAndAtGunsChange}
						title={'Vehicle Mounted & AT Guns'}>
					<VehicleMountedAndAtGunsEditor/>
				</EditorList>
				<EditorList
						emptyItem={({
							name  : ' ',
							cost  : '1/2',
							weight: '1/5',
							notes : ' ',
							era   : ' ',
							kind  : ' '
						})}
						id={'ammunitionEditorList'}
						list={this.state.ammunition}
						onChange={this.ammunitionChange}
						title={'Ammunition'}>
					<AmmunitionEditor/>
				</EditorList>
				<EditorList
						emptyItem={({
							name           : ' ',
							description    : ' ',
							cost           : 1,
							military       : false,
							weight         : 1,
							shortRange     : 1,
							mediumRange    : 2,
							longRange      : 3,
							armorPiercing  : 1,
							rateOfFire     : 1,
							minimumStrength: ' ',
							burstTemplate  : ' ',
							notes          : ' ',
							era            : ' ',
							kind           : ' '
						})}
						id={'specialWeaponsEditorList'}
						list={this.state.specialWeapons}
						onChange={this.specialWeaponsChange}
						title={'Special Weapons'}>
					<SpecialWeaponsEditor/>
				</EditorList>
				<h1>Vehicles</h1>
				<EditorList
						emptyItem={({
							name        : ' ',
							description : ' ',
							acceleration: 1,
							topSpeed    : 1,
							toughness   : 2,
							armor       : 1,
							minimumCost : 1,
							maximumCost : 2,
							notes       : ' '
						})}
						id={'groundVehiclesEditorList'}
						list={this.state.groundVehicles}
						onChange={this.groundVehiclesChange}
						title={'Ground Vehicles'}>
					<BaseVehicleEditor/>
				</EditorList>
				<EditorList
						emptyItem={({
							name        : ' ',
							description : ' ',
							acceleration: 1,
							topSpeed    : 1,
							toughness   : 2,
							armor       : 1,
							minimumCost : 1,
							maximumCost : 2,
							notes       : ' '
						})}
						id={'watercraftEditorList'}
						list={this.state.watercraft}
						onChange={this.watercraftChange}
						title={'Watercraft'}>
					<WatercraftEditor/>
				</EditorList>
				<EditorList
						emptyItem={({
							name        : ' ',
							description : ' ',
							acceleration: 1,
							topSpeed    : 1,
							toughness   : 2,
							armor       : 1,
							minimumCost : 1,
							maximumCost : 2,
							notes       : ' '
						})}
						id={'aircraftEditorList'}
						list={this.state.aircraft}
						onChange={this.aircraftChange}
						title={'Aircraft'}>
					<AircraftEditor/>
				</EditorList>
				<h1>Powers</h1>
				<EditorList
						emptyItem={({
							name          : ' ',
							description   : ' ',
							skillName     : ' ',
							attribute     : ' ',
							startingPowers: 2
						})}
						id={'arcaneBackgroundEditorList'}
						list={this.state.arcaneBackgrounds}
						onChange={this.arcaneBackgroundChange}
						title={'Arcane Backgrounds'}>
					<ArcaneBackgroundEditor/>
				</EditorList>
				<EditorList
						emptyItem={({
							name       : ' ',
							description: ' ',
							effects    : []
						})}
						id={'trappingsAndEffectsEditorList'}
						list={this.state.trappingsAndEffects}
						onChange={this.trappingsAndEffectsChange}
						title={'Trappings & Effects'}>
					<TrappingsAndEffectsEditor/>
				</EditorList>
				<EditorList
						emptyItem={({
							name       : ' ',
							description: ' ',
							rank       : ' ',
							powerPoints: 1,
							range      : ' ',
							duration   : ' '
						})}
						id={'powersEditorList'}
						list={this.state.powers}
						onChange={this.powersChange}
						title={'Powers'}>
					<PowerEditor/>
				</EditorList>
				<EditorList
						emptyItem={({
							agility           : {dice: 'd4', bonus: 0},
							animalIntelligence: false,
							name              : ' ',
							charisma          : 0,
							description       : ' ',
							pace              : 6,
							skills            : [],
							smarts            : {dice: 'd4', bonus: 0},
							specialAttributes : [],
							spirit            : {dice: 'd4', bonus: 0},
							strength          : {dice: 'd4', bonus: 0},
							vigor             : {dice: 'd4', bonus: 0},
							skills            : []
						})}
						id={'beastsEditorList'}
						list={this.state.beasts}
						onChange={this.beastsChange}
						headingLevel={1}
						title={'Beasts'}>
					<BeastEditor skillsAvailable={this.state.skills}/>
				</EditorList>
				<h1>Characters</h1>
				<button id={'savePlotPointButton'} type={'submit'} className={'btn btn-default'} onClick={this.save}>Save
				</button>
				<button id={'cancelPlotPointButton'} type={'cancel'} className={'btn btn-default'}
				        onClick={this.cancel}>Cancel
				</button>
			</form>
		</div>;
	}
}

export default withRouter(PlotPointEditor);

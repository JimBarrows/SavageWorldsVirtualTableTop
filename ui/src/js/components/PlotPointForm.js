import {DangerAlert, TextAreaFormGroup, TextFormGroup} from "bootstrap-react-components";
import React from "react";
import {EdgeList} from "./EdgeList";
import {EdgeTypeList} from "./EdgeType";
import {HindranceList} from "./Hindrance";
import {RaceList} from "../../../../ui-web/src/components/Race";
import {SettingRuleList} from "./SettingRule";
import {SkillList} from "./Skill";
import {MundaneItemList} from "./Gear/MundaneItem";
import {HandWeaponList} from "./Gear/Weapon/HandWeapon";
import {RangedWeaponList} from "./Gear/Weapon/RangedWeapon";
import {VehicleMountedAndAtGunList} from "./Gear/Weapon/VehicleMountedAndAtGun";
import {AmmunitionList} from "./Gear/Weapon/Ammunition";
import {SpecialWeaponList} from "./Gear/Weapon/SpecialWeapon";
import {ArmorList} from "./Gear/Armor";
import {VehicleList} from "./Gear/Vehicle";
import {ArcaneBackgroundList} from "./Powers/ArcaneBackground";
import {TrappingList} from "./Powers/Trapping";
import {PowerList} from "./Powers/Power";

class PlotPointForm extends React.Component {

	ammunitionChange(ammunition) {
		this.setState({
			ammunition
		});
	}

	arcaneBackgroundsChange(arcaneBackgrounds) {
		this.setState({
			arcaneBackgrounds
		});
	}

	armorChange(armor) {
		this.setState({
			armor
		});
	}

	cancel(event) {
		event.preventDefault();
		this.props.onCancel();
	}

	componentWillMount() {
		this.propsToState(this.props);
	}

	componentWillReceiveProps(nextProps) {
		this.propsToState(nextProps);
	}

	constructor(props) {
		super(props);
		this.state = {
			_id: '',
			description: "",
			name: "",
			settingRules: [],
			skillDescriptions: [],
			edges: [],
			edgeTypes: [],
			hindrances: [],
			mundaneItems: [],
			handWeapons: [],
			rangedWeapons: [],
			vehicleMountedAndAtGuns: [],
			ammunition: [],
			specialWeapons: [],
			armor: [],
			vehicles: [],
			arcaneBackgrounds: [],
			trappings: [],
			powers: []
		}
	}

	descriptionChange(event) {
		this.setState({
			description: event.target.value
		})
	}

	edgesChange(edges) {
		this.setState({
			edges
		});
	}

	edgeTypesChange(edgeTypes) {
		this.setState({
			edgeTypes
		});
	}

	handWeaponsChange(handWeapons) {
		this.setState({
			handWeapons
		});
	}

	hindrancesChange(hindrances) {
		this.setState({
			hindrances
		});
	}

	mundaneItemsChange(mundaneItems) {
		this.setState({
			mundaneItems
		});
	}

	nameChange(event) {
		this.setState({
			name: event.target.value
		});
	}

	powersChange(powers) {
		this.setState({
			powers
		})
	}

	propsToState(props) {
		let {
				    name              = "", description = "", _id = "", settingRules = [], races = [],
				    skillDescriptions = [], edges = [], edgeTypes = [], hindrances = [], mundaneItems = [],
				    handWeapons       = [], rangedWeapons = [], vehicleMountedAndAtGuns = [], ammunition = [],
				    specialWeapons    = [], armor = [], vehicles = [],
				    arcaneBackgrounds = [], trappings = [], powers = []
		    }                     = props.plotPoint;
		this.setState({
			name,
			description,
			_id,
			settingRules,
			races,
			skillDescriptions,
			edges,
			edgeTypes,
			hindrances,
			mundaneItems,
			handWeapons,
			rangedWeapons,
			vehicleMountedAndAtGuns,
			ammunition,
			specialWeapons,
			armor,
			vehicles,
			arcaneBackgrounds,
			trappings,
			powers
		});
	}

	rangedWeaponsChange(rangedWeapons) {
		this.setState({
			rangedWeapons
		})
	}

	racesChange(races) {
		this.setState({
			races
		});
	}

	render() {
		let {
				    edges, edgeTypes, error, description, handWeapons,
				    hindrances, mundaneItems, name, races, rangedWeapons,
				    settingRules, skillDescriptions, vehicleMountedAndAtGuns, ammunition,
				    specialWeapons, armor, vehicles, arcaneBackgrounds, trappings,
				    powers
		    } = this.state;
		return (
				<form id="plotPointForm">
					<DangerAlert error={error}/>
					<TextFormGroup
							id="plotPointName"
							error={error}
							label="Name"
							name="name"
							onChange={this.nameChange.bind(this)}
							value={name}
					/>
					<TextAreaFormGroup label="Description" id="description"
					                   onChange={this.descriptionChange.bind(this)} value={description}/>
					<SettingRuleList list={settingRules}
					                 onListChange={this.settingRulesChange.bind(this)}
					                 allowEditing={true}/>
					<RaceList list={races}
					          onListChange={this.racesChange.bind(this)}
					          allowEditing={true}/>
					<SkillList list={skillDescriptions} onListChange={this.skillDescriptionsChange.bind(this)}
					           allowEditing={true}/>
					<EdgeTypeList list={edgeTypes} onListChange={this.edgeTypesChange.bind(this)} allowEditing={true}/>
					<EdgeList list={edges} onListChange={this.edgesChange.bind(this)} allowEditing={true}/>
					<HindranceList list={hindrances} onListChange={this.hindrancesChange.bind(this)} allowEditing={true}/>
					<h1>Gear</h1>
					<MundaneItemList list={mundaneItems} onListChange={this.mundaneItemsChange.bind(this)} allowEditing={true}/>
					<h2>Weapons</h2>
					<HandWeaponList list={handWeapons} onListChange={this.handWeaponsChange.bind(this)} allowEditing={true}/>
					<RangedWeaponList list={rangedWeapons} onListChange={this.rangedWeaponsChange.bind(this)}
					                  allowEditing={true}/>
					<VehicleMountedAndAtGunList list={vehicleMountedAndAtGuns}
					                            onListChange={this.vehicleMountedAndAtGunsChange.bind(this)} allowEditing={true}/>
					<AmmunitionList list={ammunition} onListChange={this.ammunitionChange.bind(this)} allowEditing={true}/>
					<SpecialWeaponList list={specialWeapons} onListChange={this.specialWeaponsChange.bind(this)}
					                   allowEditing={true}/>
					<ArmorList list={armor} onListChange={this.armorChange.bind(this)}
					           allowEditing={true}/>
					<VehicleList list={vehicles} onListChange={this.vehicleChange.bind(this)} allowEditing={true}/>
					<h1>Powers</h1>
					<ArcaneBackgroundList list={arcaneBackgrounds} onListChange={this.arcaneBackgroundsChange.bind(this)}
					                      allowEditing={true}/>
					<TrappingList list={trappings} onListChange={this.trappingsChange.bind(this)}
					              allowEditing={true}/>
					<PowerList list={powers} onListChange={this.powersChange.bind(this)} allowEditing={true}/>
					<button type="button" class="btn btn-primary" onClick={this.submit.bind(this)}>Save</button>
					<button type="button" class="btn btn-default" onClick={this.cancel.bind(this)}>Cancel</button>
				</form>
		);
	}

	settingRulesChange(newRules) {
		this.setState({
			settingRules: newRules
		});
	}

	skillDescriptionsChange(skillDescriptions) {
		this.setState({
			skillDescriptions
		});
	}

	specialWeaponsChange(specialWeapons) {
		this.setState({
			specialWeapons
		});
	}

	submit(event) {
		event.preventDefault();
		let {
				    name              = "", description = "", _id = "", settingRules = [], races = [],
				    skillDescriptions = [], edges = [], edgeTypes = [], hindrances = [], mundaneItems = [],
				    handWeapons       = [], rangedWeapons = [], vehicleMountedAndAtGuns = [], ammunition = [],
				    specialWeapons    = [], armor = [], vehicles = [], arcaneBackgrounds = [], trappings = [],
				    powers            = []
		    }                     = this.state;
		this.props.onSubmit({
			_id,
			description,
			name,
			settingRules,
			races,
			skillDescriptions,
			edges,
			edgeTypes,
			hindrances,
			mundaneItems,
			handWeapons,
			rangedWeapons,
			vehicleMountedAndAtGuns,
			ammunition,
			specialWeapons,
			armor,
			vehicles,
			arcaneBackgrounds,
			trappings,
			powers
		});
	}

	trappingsChange(trappings) {
		this.setState({
			trappings
		});
	}

	vehicleChange(vehicles) {
		this.setState({
			vehicles
		});
	}

	vehicleMountedAndAtGunsChange(vehicleMountedAndAtGuns) {
		this.setState({
			vehicleMountedAndAtGuns
		});
	}
}
export default PlotPointForm;
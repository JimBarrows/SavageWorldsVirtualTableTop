import {API} from 'aws-amplify';
import {PageHeader} from 'bootstrap-react-components';
import React from 'react';
import {withRouter} from 'react-router';
import ArmorEditor from '../components/ArmorEditor';
import EdgeEditor from '../components/EdgeEditor';
import EditorList from '../components/EditorList';
import HandWeaponEditor from '../components/HandWeaponEditor';
import HindranceEditor from '../components/HindranceEditor';
import MundaneItemEditor from '../components/MundaneItemEditor';
import NumberFormGroup from '../components/NumberFormGroup';
import RaceEditor from '../components/RaceEditor';
import RangedWeaponEditor from '../components/RangedWeaponEditor';
import SkillEditor from '../components/SkillEditor';
import TextAreaFormGroup from '../components/TextAreaFormGroup';
import TextFormGroup from '../components/TextFormGroup';


class PlotPointEditor extends React.Component {

	static defaultProps = {
		id: 'PlotPointEditorPage'
	};

	state = {
		armor                 : [],
		description           : '',
		edges                 : [],
		handWeapons           : [],
		hindrances            : [],
		maximumAttributePoints: 5,
		maximumMajorHindrances: 1,
		maximumMinorHindrances: 2,
		maximumSkillPoints    : 15,
		mundaneItems          : [],
		name                  : '',
		races                 : [],
		rangedWeapons         : [],
		skills                : []
	};

	onArmorChange                = armor => this.setState({armor});
	cancel                       = e => {
		e.preventDefault();
		this.props.cancel();
	};
	descriptionChange            = e => this.setState({description: e.target.value});
	maximumAttributePointsChange = e => this.setState({maximumAttributePoints: parseInt(e.target.value, 10)});
	maximumMajorHindrancesChange = e => this.setState({maximumMajorHindrances: parseInt(e.target.value, 10)});
	maximumMinorHindrancesChange = e => this.setState({maximumMinorHindrances: parseInt(e.target.value, 10)});
	maximumSkillPointsChange     = e => this.setState({maximumSkillPoints: parseInt(e.target.value, 10)});
	nameChange                   = e => this.setState({name: e.target.value});
	onEdgeListChange             = edges => this.setState({edges});
	onHandWeaponsChange          = handWeapons => this.setState({handWeapons});
	onRacesChange                = races => this.setState({races});
	onRangedWeaponsChange        = rangedWeapons => this.setState({rangedWeapons});
	onSkillsChange               = skills => this.setState({skills});
	onMundaneItemsChange         = mundaneItems => this.setState({mundaneItems});
	onHandWeaponsChange          = handWeapons => this.setState({handWeapons});

	save = async e => {
		e.preventDefault();
		let toSave = {
			armor                 : this.state.armor,
			description           : this.state.description,
			edges                 : this.state.edges,
			handWeapons           : this.state.handWeapons,
			hindrances            : this.state.hindrances,
			maximumAttributePoints: this.state.maximumAttributePoints,
			maximumMajorHindrances: this.state.maximumMajorHindrances,
			maximumMinorHindrances: this.state.maximumMinorHindrances,
			maximumSkillPoints    : this.state.maximumSkillPoints,
			mundaneItems          : this.state.mundaneItems,
			name                  : this.state.name,
			races                 : this.state.races,
			rangedWeapons         : this.state.rangedWeapons,
			skills                : this.state.skills
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
				<EditorList emptyItem={({name: '', description: '', abilities: []})}
				            id={'RaceEditorList'}
				            list={this.state.races}
				            onChange={this.onRacesChange}
				            title={'Races'}>
					<RaceEditor/>
				</EditorList>
				<EditorList emptyItem={({name: '', description: '', abilities: []})}
				            id={'SkillEditorList'}
				            list={this.state.skills}
				            onChange={this.onSkillsChange}
				            title={'Skills'}>
					<SkillEditor/>
				</EditorList>
				<EditorList emptyItem={({name: '', description: '', abilities: []})}
				            id={'HindrancesEditorList'}
				            list={this.state.hindrances}
				            onChange={this.onHindrancesChange}
				            title={'Hindrances'}>
					<HindranceEditor/>
				</EditorList>
				<EditorList emptyItem={({name: '', description: '', category: ''})}
				            id={'EdgeEditorList'}
				            list={this.state.edges}
				            onChange={this.onEdgeListChange}
				            title={'Edges'}>
					<EdgeEditor/>
				</EditorList>
				<h1>Gear</h1>
				<EditorList emptyItem={({name: '', description: '', cost: 1, weight: 1})}
				            id={'MundaneItemEditorList'}
				            list={this.state.mundaneItems}
				            onChange={this.onMundaneItemsChange}
				            title={'Mundane Items'}>
					<MundaneItemEditor/>
				</EditorList>
				<EditorList
						emptyItem={({name: '', description: '', cost: 1, weight: 1, damage: '', notes: '', era: '', kind: ''})}
						id={'HandWeaponsEditorList'}
						list={this.state.handWeapons}
						onChange={this.onHandWeaponsChange}
						title={'Hand Weapons'}>
					<HandWeaponEditor/>
				</EditorList>
				<EditorList
						emptyItem={({name: '', description: '', cost: 1, weight: 1, armor: '', notes: '', era: '', kind: ''})}
						id={'ArmorEditorList'}
						list={this.state.armor}
						onChange={this.onArmorChange}
						title={'Armor'}>
					<ArmorEditor/>
				</EditorList>
				<h3>Ranged Weapons</h3>
				<EditorList
						emptyItem={({
							name           : '',
							description    : '',
							cost           : 1,
							weight         : 1,
							shortRange     : 1,
							mediumRange    : 2,
							longRange      : 3,
							damage         : '',
							rateOfFire     : 1,
							shots          : 1,
							minimumStrength: '',
							notes          : '',
							era            : '',
							kind           : ''
						})}
						id={'RangedWeaponEditorList'}
						list={this.state.rangedWeapons}
						onChange={this.onRangedWeaponsChange}
						title={'Ranged Weapons'}>
					<RangedWeaponEditor/>
				</EditorList>
				<h3>Vehicle Mounted & AT Guns</h3>
				<h3>Ammunition</h3>
				<h3>Special Weapons</h3>
				<h3>Vehicles</h3>
				<h3>Watercraft</h3>
				<h3>Aircraft</h3>
				<h2>Powers</h2>
				<h2>Beasts</h2>
				<h2>Characters</h2>
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

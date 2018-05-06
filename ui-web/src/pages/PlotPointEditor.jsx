import {API} from 'aws-amplify';
import {PageHeader} from 'bootstrap-react-components';
import React from 'react';
import {withRouter} from 'react-router';
import NumberFormGroup from '../components/NumberFormGroup';
import {RaceEditor} from '../components/Race';
import SkillEditor from '../components/SkillEditor';
import TextAreaFormGroup from '../components/TextAreaFormGroup';
import TextFormGroup from '../components/TextFormGroup';


class PlotPointEditor extends React.Component {

	static defaultProps = {
		id: 'PlotPointEditorPage'
	};

	state = {
		description           : '',
		maximumAttributePoints: 5,
		maximumMajorHindrances: 1,
		maximumMinorHindrances: 2,
		maximumSkillPoints    : 15,
		name                  : '',
		races                 : [],
		skills                : []
	};

	addRace  = (event) => {
		event.preventDefault();
		this.setState({
			races: [{name: '', description: '', abilities: []}, ...this.state.races]
		});
	};
	addSkill = event => {
		event.preventDefault();
		this.setState({
			skills: [{name: '', attribute: 'Ability', description: ''}, ...this.state.skills]
		});
	};
	cancel   = e => {
		e.preventDefault();
		this.props.cancel();
	};
	save     = async e => {
		e.preventDefault();
		let toSave = {
			description           : this.state.description,
			maximumAttributePoints: this.state.maximumAttributePoints,
			maximumMajorHindrances: this.state.maximumMajorHindrances,
			maximumMinorHindrances: this.state.maximumMinorHindrances,
			maximumSkillPoints    : this.state.maximumSkillPoints,
			name                  : this.state.name,
			races                 : this.state.races,
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

	descriptionChange            = e => this.setState({description: e.target.value});
	skillChange                  = (skill, index) => this.setState({skills: this.state.skills.map((s, i) => i === index ? skill : s)});
	maximumAttributePointsChange = e => this.setState({maximumAttributePoints: parseInt(e.target.value, 10)});
	maximumMajorHindrancesChange = e => this.setState({maximumMajorHindrances: parseInt(e.target.value, 10)});
	maximumMinorHindrancesChange = e => this.setState({maximumMinorHindrances: parseInt(e.target.value, 10)});
	maximumSkillPointsChange     = e => this.setState({maximumSkillPoints: parseInt(e.target.value, 10)});
	nameChange                   = e => this.setState({name: e.target.value});
	raceChange                   = (race, index) => this.setState({races: this.state.races.map((r, i) => i === index ? race : r)});
	raceDelete                   = (index) => this.setState({races: this.state.races.filter((r, i) => i !== index)});
	races                        = () => {
		if (this.state.races.length === 0) {
			return <p>No races</p>;
		} else {
			return this.state.races.map((race, index) =>
					<RaceEditor key={index} index={index} race={race} onChange={this.raceChange} onDelete={this.raceDelete}/>);
		}
	};
	skillDelete                  = index => this.setState({skills: this.state.skills.filter((r, i) => i !== index)});
	skills                       = () => {
		if (this.state.skills.length === 0) {
			return <p>No skills</p>;
		} else {
			return this.state.skills.map((skill, index) =>
					<SkillEditor key={index} index={index} skill={skill} onChange={this.skillChange}
					             onDelete={this.skillDelete}/>);
		}
	};

	async componentDidMount() {
		if (this.props.match.params.name) {
			let plotPoint = await API.get('PlotPointsCRUD', `/PlotPoints/object/${this.props.match.params.name}`);
			this.setState({
				...plotPoint
			});
		}
	}

	render() {
		return <div id={this.props.id}>
			<PageHeader id={this.props.id}><h1>Plot Point Editor</h1></PageHeader>
			<form id='plotPointForm'>
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
				<h2>Races</h2>
				<button id={'addRaceButton'} className="btn btn-default" onClick={this.addRace}>Add</button>
				{this.races()}
				<h2>Skills</h2>
				<button id={'addSkillButton'} className="btn btn-default" onClick={this.addSkill}>Add</button>
				{this.skills()}
				<h2>Hindrances</h2>
				<h2>Edges</h2>
				<h2>Gear</h2>
				<h3>Mundane Items</h3>
				<h3>Hand Weapons</h3>
				<h3>Armor</h3>
				<h3>Ranged Weapons</h3>
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

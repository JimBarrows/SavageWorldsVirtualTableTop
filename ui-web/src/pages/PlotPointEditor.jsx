import {API} from 'aws-amplify';
import {PageHeader} from 'bootstrap-react-components';
import React from 'react';
import {withRouter} from 'react-router';
import NumberFormGroup from '../components/NumberFormGroup';
import {RaceEditor} from '../components/Race';
import TextAreaFormGroup from '../components/TextAreaFormGroup';
import TextFormGroup from '../components/TextFormGroup';


class PlotPointEditor extends React.Component {

	static defaultProps = {
		id: 'PlotPointEditorPage'
	};

	state = {
		name                  : '',
		description           : '',
		maximumAttributePoints: 5,
		maximumMajorHindrances: 1,
		maximumMinorHindrances: 2,
		maximumSkillPoints    : 15,
		races                 : []
	};

	addRace = (event) => {
		event.preventDefault();
		this.setState({
			races: [{name: '', description: '', abilities: []}, ...this.state.races]
		});
	};


	cancel = e => {
		e.preventDefault();
		this.props.cancel();
	};

	maximumAttributePointsChange = e => this.setState({maximumAttributePoints: parseInt(e.target.value, 10)});
	descriptionChange            = e => this.setState({description: e.target.value});
	maximumMajorHindrancesChange = e => this.setState({maximumMajorHindrances: parseInt(e.target.value, 10)});
	maximumMinorHindrancesChange = e => this.setState({maximumMinorHindrances: parseInt(e.target.value, 10)});
	maximumSkillPointsChange     = e => this.setState({maximumSkillPoints: parseInt(e.target.value, 10)});
	nameChange                   = e => this.setState({name: e.target.value});
	raceChange                   = (race, index) => this.setState({races: this.state.races.map((r, i) => i === index ? race : r)});
	races                        = () => {
		if (this.state.races.length === 0) {
			return <p>No races</p>;
		} else {
			return this.state.races.map((race, index) =>
					<RaceEditor key={index} index={index} race={race} onChange={this.raceChange}/>);
		}
	};
	save                         = async e => {
		e.preventDefault();
		let plotPoint = await API.post('PlotPointsCRUD', `/PlotPoints`, {
			body: {
				name                  : this.state.name,
				description           : this.state.description,
				maximumAttributePoints: this.state.maximumAttributePoints,
				maximumMajorHindrances: this.state.maximumMajorHindrances,
				maximumMinorHindrances: this.state.maximumMinorHindrances,
				maximumSkillPoints    : this.state.maximumSkillPoints,
				races                 : this.state.races
			}
		});
		this.props.history.push('/');
	};

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
				<button id={'savePlotPointButton'} type={'submit'} className={'btn btn-default'} onClick={this.save}>Save
				</button>
				<button id={'cancelPlotPointButton'} type={'cancel'} className={'btn btn-default'}
				        onClick={this.cancel}>Cancel
				</button>
			</form>
		</div>;
	}

	async componentDidMount() {
		if (this.props.match.params.name) {
			let plotPoint = await API.get('PlotPointsCRUD', `/PlotPoints/object/${this.props.match.params.name}`);
			this.setState({
				...plotPoint
			});
		}
	}
}

export default withRouter(PlotPointEditor);

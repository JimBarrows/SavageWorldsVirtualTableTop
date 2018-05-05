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
		this.props.addRace();
	};

	addRacialAbility             = (indexOfRace) => this.props.addRacialAbility(indexOfRace);
	maximumAttributePointsChange = e => this.setState({maximumAttributePoints: parseInt(e.target.value, 10)});

	cancel = e => {
		e.preventDefault();
		this.props.cancel();
	};

	descriptionChange            = e => this.props.descriptionChange(e.target.value);
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
					<RaceEditor key={index} index={index} race={race}
					            addRacialAbility={this.addRacialAbility}
					            deleteRacialAbility={this.deleteRacialAbility}
					            onChange={this.raceChange}/>);
		}
	};

	async componentDidMount() {
		if (this.props.match.params.name) {
			let plotPoint = await API.get('PlotPointsCRUD', `/plotPoints/object/${this.props.match.params.name}`);
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
				<button id={'savePlotPointButton'} type={'submit'} className={'btn btn-default'} onClick={this.save}>Save
				</button>
				<button id={'cancelPlotPointButton'} type={'cancel'} className={'btn btn-default'}
				        onClick={this.cancel}>Cancel
				</button>
			</form>
		</div>;
	}

	save = e => {
		e.preventDefault();
		this.props.savePlotPoint();
	};
}

export default withRouter(PlotPointEditor);

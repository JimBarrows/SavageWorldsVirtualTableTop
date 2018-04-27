import {PageHeader} from 'bootstrap-react-components';
import React from 'react';
import {connect} from 'react-redux';
import {
	cancelChanges,
	descriptionChange, loadPlotPoint,
	maximumAttributePointsChange,
	maximumMajorHindrancesChange, maximumMinorHindrancesChange,
	maximumSkillPointsChange, nameChange, newPlotPoint, raceChange, savePlotPoint
} from '../actions';
import NumberFormGroup from '../components/NumberFormGroup';
import {RaceEditor} from '../components/Race';
import TextAreaFormGroup from '../components/TextAreaFormGroup';
import TextFormGroup from '../components/TextFormGroup';


class PlotPointEditor extends React.Component {

	static defaultProps = {
		id: 'PlotPointEditorPage'
	};

	componentDidMount() {
		if (this.props.match.params.name) {
			this.props.loadPlotPoint(this.props.match.params.name);
		} else {
			this.props.newPlotPoint();
		}
	}

	cancel = e => {
		e.preventDefault();
		this.props.cancel();
	};

	descriptionChange = e => this.props.descriptionChange(e.target.value);

	maximumAttributePointsChange = e => this.props.maximumAttributePointsChange(e.target.value);

	maximumMajorHindrancesChange = e => this.props.maximumMajorHindrancesChange(e.target.value);

	maximumMinorHindrancesChange = e => this.props.maximumMinorHindrancesChange(e.target.value);

	maximumSkillPointsChange = e => this.props.maximumSkillPointsChange(e.target.value);

	nameChange = e => this.props.nameChange(e.target.value);

	raceChange = (race, index) => this.props.raceChange(race, index);

	render() {
		return <div id={this.props.id}>
			<PageHeader id={this.props.id}><h1>Plotpoint Editor</h1></PageHeader>
			<form id='plotPointForm'>
				<TextFormGroup id='plotPointName' label='Name' onChange={this.nameChange} required={true}
				               value={this.props.name}/>
				<TextAreaFormGroup id={'plotPointDescription'} label={'Description'} onChange={this.descriptionChange}
				                   value={this.props.description}/>
				<NumberFormGroup id={'maximumAttributePoints'} label={'Maximum Attribute Points'}
				                 onChange={this.maximumAttributePointsChange} required={true}
				                 value={this.props.maximumAttributePoints}/>
				<NumberFormGroup id={'maximumMajorHindrances'} label={'Maximum Number of Major Hindrances'}
				                 onChange={this.maximumMajorHindrancesChange} required={true}
				                 value={this.props.maximumMajorHindrances}/>
				<NumberFormGroup id={'maximumMinorHindrances'} label={'Maximum Number of Minor Hindrances'}
				                 onChange={this.maximumMinorHindrancesChange} required={true}
				                 value={this.props.maximumMinorHindrances}/>
				<NumberFormGroup id={'maximumSkillPoints'} label={'Maximum Skill Points'}
				                 onChange={this.maximumSkillPointsChange} required={true}
				                 value={this.props.maximumSkillPoints}/>
				<h2>Races</h2>
				{this.props.races.map((race, index) =>
						<RaceEditor key={index} index={index} race={race}
						            onChange={this.raceChange}/>)}
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

const mapStateToProps = (state) => {
	return {

		description           : state.PlotPoint.description,
		maximumAttributePoints: state.PlotPoint.maximumAttributePoints,
		maximumMajorHindrances: state.PlotPoint.maximumMajorHindrances,
		maximumMinorHindrances: state.PlotPoint.maximumMinorHindrances,
		maximumSkillPoints    : state.PlotPoint.maximumSkillPoints,
		name                  : state.PlotPoint.name,
		races                 : state.PlotPoint.races
	};
};

const mapDispatchToProps = (dispatch) => {
	return {
		cancel                      : () => dispatch(cancelChanges()),
		descriptionChange           : (description) => dispatch(descriptionChange(description)),
		loadPlotPoint               : (name) => dispatch(loadPlotPoint(name)),
		maximumAttributePointsChange: (maximumAttributePoints) => dispatch(maximumAttributePointsChange(maximumAttributePoints)),
		maximumMajorHindrancesChange: (maximumMajorHindrances) => dispatch(maximumMajorHindrancesChange(maximumMajorHindrances)),
		maximumMinorHindrancesChange: (maximumMinorHindrances) => dispatch(maximumMinorHindrancesChange(maximumMinorHindrances)),
		maximumSkillPointsChange    : (maximumSkillPoints) => dispatch(maximumSkillPointsChange(maximumSkillPoints)),
		nameChange                  : (name) => dispatch(nameChange(name)),
		newPlotPoint                : () => dispatch(newPlotPoint()),
		raceChange                  : (race, index) => dispatch(raceChange(race, index)),
		savePlotPoint               : () => dispatch(savePlotPoint())
	};
};

export default connect(mapStateToProps, mapDispatchToProps)(PlotPointEditor);

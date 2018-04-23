import axios from 'axios';
import {NumberFormGroup, PageHeader, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components';
import React from 'react';
import {connect} from 'react-redux';
import {withRouter} from 'react-router';
import {
	cancelChanges,
	descriptionChange,
	maximumAttributePointsChange,
	maximumMajorHindrancesChange, maximumMinorHindrancesChange,
	maximumSkillPointsChange
} from '../actions';
import {checkHttpStatus, parseJSON} from '../utils';

class PlotPointEditor extends React.Component {

	static defaultProps = {
		id: 'PlotPointEditorPage'
	};

	constructor(props) {
		super(props);
		this.cancel                       = this.cancel.bind(this);
		this.descriptionChange            = this.descriptionChange.bind(this);
		this.maximumAttributePointsChange = this.maximumAttributePointsChange.bind(this);
		this.maximumMajorHindrancesChange = this.maximumMajorHindrancesChange.bind(this);
		this.maximumMinorHindrancesChange = this.maximumMinorHindrancesChange.bind(this);
		this.maximumSkillPointsChange     = this.maximumSkillPointsChange.bind(this);
		this.nameChange                   = this.nameChange.bind(this);
		this.save                         = this.save.bind(this);
	}

	componentDidMount() {

	}

	cancel(e) {
		e.preventDefault();
		this.props.cancel();
	}

	descriptionChange(e) {
		this.props.descriptionChange(e.target.value);
	}

	maximumAttributePointsChange(e) {
		this.props.maximumAttributePointsChange(e.target.value);
	}

	maximumMajorHindrancesChange(e) {
		this.props.maximumMajorHindrancesChange(e.target.value);
	}

	maximumMinorHindrancesChange(e) {
		this.props.maximumMinorHindrancesChange(e.target.value);
	}

	maximumSkillPointsChange(e) {
		this.props.maximumSkillPointsChange(e.target.value);
	}

	nameChange(e) {
		this.setState({
			name: e.target.value
		});
	}

	render() {
		return <div id={this.props.id}>
			<PageHeader id={this.props.id}><h1>Plotpoint Editor</h1></PageHeader>
			<form id='plotPointForm'>
				<TextFormGroup id='plotPointName' label='Name' onChange={this.nameChange} required={true}
				               value={this.props.name}/>
				<TextAreaFormGroup id={'plotPointDescription'} label={'Description'} onChange={this.descriptionChange}
				                   value={this.props.description}/>
				<NumberFormGroup id={'maximumAttributePoints'} label={'Maximum Attribute Points'}
				                 onChange={this.maximumAttributePointsChange} value={this.props.maximumAttributePoints}/>
				<NumberFormGroup id={'maximumMajorHindrances'} label={'Maximum Number of Major Hindrances'}
				                 onChange={this.maximumMajorHindrancesChange} value={this.props.maximumMajorHindrances}/>
				<NumberFormGroup id={'maximumMinorHindrances'} label={'Maximum Number of Minor Hindrances'}
				                 onChange={this.maximumMinorHindrancesChange} value={this.props.maximumMinorHindrances}/>
				<NumberFormGroup id={'maximumSkillPoints'} label={'Maximum Skill Points'}
				                 onChange={this.maximumSkillPointsChange} value={this.props.maximumSkillPoints}/>
				<button id={'savePlotPointButton'} type={'submit'} className={'btn btn-default'} onClick={this.save}>Save
				</button>
				<button id={'cancelPlotPointButton'} type={'cancel'} className={'btn btn-default'}
				        onClick={this.cancel}>Cancel
				</button>
			</form>
		</div>;
	}

	save(e) {
		e.preventDefault();
		let {
			    name,
			    description,
			    maximumMinorHindrances,
			    maximumMajorHindrances,
			    maximumAttributePoints,
			    maximumSkillPoints
		    } = this.state;

		axios.post('/api/plotPoints', {
			name,
			description,
			maximumMinorHindrances,
			maximumMajorHindrances,
			maximumAttributePoints,
			maximumSkillPoints
		}).then(checkHttpStatus)
				.then(parseJSON)
				.then();
	}
}

const mapStateToProps = (state) => {
	return {
		name                  : state.PlotPoint.name,
		description           : state.PlotPoint.description,
		maximumMajorHindrances: state.PlotPoint.maximumMajorHindrances,
		maximumMinorHindrances: state.PlotPoint.maximumMinorHindrances,
		maximumAttributePoints: state.PlotPoint.maximumAttributePoints,
		maximumSkillPoints    : state.PlotPoint.maximumSkillPoints
	};
};

const mapDispatchToProps = (dispatch) => {
	return {
		cancel                      : () => dispatch(cancelChanges()),
		descriptionChange           : () => dispatch(descriptionChange()),
		maximumAttributePointsChange: () => dispatch(maximumAttributePointsChange()),
		maximumMajorHindrancesChange: () => dispatch(maximumMajorHindrancesChange()),
		maximumMinorHindrancesChange: () => dispatch(maximumMinorHindrancesChange()),
		maximumSkillPointsChange    : () => dispatch(maximumSkillPointsChange())
	}
			;
};

export default withRouter(connect(mapStateToProps, mapDispatchToProps)(PlotPointEditor));

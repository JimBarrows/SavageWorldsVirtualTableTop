import {NumberFormGroup, PageHeader, TextAreaFormGroup, TextFormGroup} from 'bootstrap-react-components';
import React from 'react';
import {checkHttpStatus, parseJSON} from '../utils';
import axios from 'axios';

export default class AddPlotPoint extends React.Component {

	static defaultProps = {
		id: 'AddPlotPointPage'
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
		this.state                        = {
			name                  : '',
			description           : '',
			maximumMinorHindrances: 2,
			maximumMajorHindrances: 2,
			maximumAttributePoints: 5,
			maximumSkillPoints    : 15
		};
	}

	cancel(e) {
		e.preventDefault();
		this.setState({
			name                  : '',
			description           : '',
			maximumMinorHindrances: 2,
			maximumMajorHindrances: 2,
			maximumAttributePoints: 5,
			maximumSkillPoints    : 15
		});
	}

	descriptionChange(e) {
		this.setState({
			description: e.target.value
		});
	}

	maximumAttributePointsChange(e) {
		this.setState({
			maximumAttributePoints: e.target.value
		});
	}

	maximumMajorHindrancesChange(e) {
		this.setState({
			maximumMajorHindrances: e.target.value
		});
	}

	maximumMinorHindrancesChange(e) {
		this.setState({
			maximumMinorHindrances: e.target.value
		});
	}

	maximumSkillPointsChange(e) {
		this.setState({
			maximumSkillPoints: e.target.value
		});
	}

	nameChange(e) {
		this.setState({
			name: e.target.value
		});
	}

	render() {
		return <div id={this.props.id}>
			<PageHeader id={this.props.id}><h1>Add Plotpoint</h1></PageHeader>
			<form id='plotPointForm'>
				<TextFormGroup id='plotPointName' label='Name' onChange={this.nameChange} required={true}
				               value={this.state.name}/>
				<TextAreaFormGroup id={'plotPointDescription'} label={'Description'} onChange={this.descriptionChange}
				                   value={this.state.description}/>
				<NumberFormGroup id={'maximumMinorHindrances'} label={'Maximum Number of Minor Hindrances'}
				                 onChange={this.maximumMinorHindrancesChange} value={this.state.maximumMinorHindrances}/>
				<NumberFormGroup id={'maximumMajorHindrances'} label={'Maximum Number of Major Hindrances'}
				                 onChange={this.maximumMajorHindrancesChange} value={this.state.maximumMajorHindrances}/>
				<NumberFormGroup id={'maximumAttributePoints'} label={'Maximum Attribute Points'}
				                 onChange={this.maximumAttributePointsChange} value={this.state.maximumAttributePoints}/>
				<NumberFormGroup id={'maximumSkillPoints'} label={'Maximum Skill Points'}
				                 onChange={this.maximumSkillPointsChange} value={this.state.maximumSkillPoints}/>
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

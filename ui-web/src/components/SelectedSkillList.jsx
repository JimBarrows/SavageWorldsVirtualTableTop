import React from 'react';
import PropTypes from 'prop-types';
import SelectedSkillEditor from './SelectedSkillEditor';
import SelectFormGroup from './SelectFormGroup';

export default class SelectedSkillList extends React.Component {

	static propTypes = {
		id             : PropTypes.string.isRequired,
		skillsAvailable: PropTypes.array.isRequired,
		selectedSkills : PropTypes.array.isRequired,
		onChange       : PropTypes.func.isRequired
	};

	static defaultProps = {};

	state = {
		selected: ''
	};

	addSkill = (skill) => this.props.onChange([skill, ...this.props.selectedSkills]);

	render() {
		return (
				<div id={'SelectedSkillListComponent_' + this.props.id}>
					<h3>Skills</h3>
					<div id={'SelectedSkillListHeader_' + this.props.id} className={'row'}>
						<div className={'col-sm-4'}>
							<h4>Name</h4>
						</div>
						<div className={'col-sm-4'}>
							<h4>Rank</h4>
						</div>
						<div className={'col-sm-4'}>
							<h4>Specialization</h4>
						</div>
					</div>
					<SelectedSkillEditor id={this.props.id} selectedSkillChanged={this.addSkill}
					                     skillsAvailable={this.props.skillsAvailable}/>
				</div>
		);
	}
}


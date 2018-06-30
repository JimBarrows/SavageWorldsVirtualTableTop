import PropTypes from 'prop-types'
import React from 'react'
import SelectedSkillEditor from '../editors/SelectedSkillEditor'

export default class SelectedSkillList extends React.Component {

	static propTypes = {
		id             : PropTypes.string.isRequired,
		skillsAvailable: PropTypes.array.isRequired,
		skills : PropTypes.array.isRequired,
		onChange       : PropTypes.func.isRequired
	};

	static defaultProps = {};

	addSkill = (e) => {
		e.preventDefault();
		console.log(`addSkill(e)`);
		this.props.onChange([{  name: ' ', dice: ' ', bonus: 0, note: ' '}, ...this.props.skills])};

	skillChanged = (indexOfChange, changedSkill) => {console.log(`skillChange(${indexOfChange}, ${changedSkill})`);this.props.onChange(this.props.skills.map( (skill, index) => indexOfChange === index ? changedSkill : skill));}

	skillEditorList = () => this.props.skills.map((selectedSkill, index) => <SelectedSkillEditor key={index}
																																																id={'skill_' + index}
																																																index={index}
																																																onChange={this.skillChanged}
																																																skill={selectedSkill}
																																																skillsAvailable={this.props.skillsAvailable}/>);
	render() {
		return (
				<div id={'SelectedSkillListComponent_' + this.props.id}>
					<h3>Skills</h3>
					<button id={`addSelectedSkillButton+_${this.props.id}`} className="btn btn-default" onClick={this.addSkill}>Add</button>
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
					{this.skillEditorList()}
				</div>
		);
	}
}

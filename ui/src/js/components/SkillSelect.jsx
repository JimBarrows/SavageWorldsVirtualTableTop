import {FormGroup} from "bootstrap-react-components";
import React from "react";
import {connect} from "react-redux";

class SkillSelect extends React.Component {

	onChange(e) {
		let chosenSkill = this.props.skills.find((s) => s._id === e.target.value);
		this.props.onChange(chosenSkill);
	}

	render() {
		return (
				<FormGroup label="Skill" id="skill" required={true}>
					<select class="form-control" id="skillSelect" onChange={this.onChange.bind(this)}
					        value={this.props.value}>
						{this.props.skills.map((skill) => <option key={skill._id} value={skill._id}>{skill.name}</option>)}
					</select>
				</FormGroup>
		);
	}
}

const mapStateToProps = (state) => {
	return {
		skills: state.PlotPoint.plotPoint.skillDescriptions
	};
};

const mapDispatchToProps = (dispatch) => {
	return {};
};

export default connect(mapStateToProps, mapDispatchToProps)(SkillSelect);
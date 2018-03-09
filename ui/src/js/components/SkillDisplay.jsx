import React from "react";
import {connect} from "react-redux";

class SkillDisplay extends React.Component {

	render() {
		let {skillId, skills} = this.props;
		if (skillId._id) {
			skillId = skillId._id;
		}
		let skill = skills.find((s) => s._id === skillId);
		return (
				<span id="SkillDisplay">
					<span class="skillName">{skill.name}</span>
					<span class="skillAbility"><small>({skill.attribute})</small></span>
				</span>
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

export default connect(mapStateToProps, mapDispatchToProps)(SkillDisplay);
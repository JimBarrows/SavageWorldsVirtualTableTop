import {RowControlButtons} from "bootstrap-react-components";
import React from "react";
import SkillDisplay from "../../SkillDisplay";

class ArcaneBackgroundView extends React.Component {

	render() {
		let {_id, name, description, skill, startingPowerPoints, startingPowers, edit, remove, save, allowEditing} = this.props;
		return (
				<div class="arcaneViewPage">
					<h3>{name}{allowEditing ? <RowControlButtons id={_id}
					                                             editing={false}
					                                             edit={edit}
					                                             save={save}
					                                             remove={remove}/> : ""}
					</h3>
					<p><b>Skill: </b> <SkillDisplay skillId={skill}/> <b>Starting Power Points: </b> {startingPowerPoints} <b>Starting
						Powers: </b> {startingPowers}</p>
					{description}
				</div>
		);
	}
}

export default ArcaneBackgroundView;
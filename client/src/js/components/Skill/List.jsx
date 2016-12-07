import React from "react";
import ListManager from "../Item/List";
import SkillDescription from "./Description";
import SkillEditor from "./Editor";

class SkillList extends ListManager {

	render() {
		const {list, allowEditing} = this.state;
		let addButton = this.buttonEditOrNothing("Add Skill", <SkillEditor save={this.addToList.bind(this)}/>);

		return (
				<div id="skillList">
					<h2>Skills</h2>
					{addButton}
					{list.map((item, index) => (
							<SkillDescription key={item._id}
							                  item={item}
							                  save={this.addToList.bind(this)}
							                  update={this.updateItem.bind(this)}
							                  remove={this.removeItem.bind(this)}
							                  allowEditing={allowEditing}/>))}
				</div>
		);
	}
}

export default SkillList;
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
					<h1>Skills</h1>
					{addButton}
					<dl>
						{list.map((item, index) => (
								<SkillDescription key={item._id}
								                  item={item}
								                  save={this.updateItem.bind(this)}
								                  remove={this.removeItem.bind(this)}
								                  allowEditing={allowEditing}/>))}
					</dl>
				</div>
		);
	}
}

export default SkillList;
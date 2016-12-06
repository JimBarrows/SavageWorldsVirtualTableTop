import React from "react";
import ListManager from "./ItemList/ItemList";
import SkillDescription from "./SkillDescription";

class SkillList extends ListManager {

	render() {
		const {list, allowEditing} = this.state;
		let addButton              = this.buttonEditOrNothing("Add Skill", <SkillDescription adding={true}
		                                                                                     save={this.addToList.bind(this)}/>);
		return (
				<div id="skillList">
					<h2>Skills</h2>
					{addButton}
					{list.map((item, index) => (
							<SkillDescription key={item._id || `${item.name.replace(" ", "_")}_${index}`}
							                  item={item}
							                  index={index}
							                  save={this.addToList.bind(this)}
							                  update={this.updateItem.bind(this)}
							                  remove={this.removeItem.bind(this)}
							                  allowEditing={allowEditing}/>))}
				</div>
		);
	}
}

export default SkillList;
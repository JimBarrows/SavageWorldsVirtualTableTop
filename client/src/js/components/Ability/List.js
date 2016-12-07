import React from "react";
import AbilityDescription from "./Description";
import AbilityEditor from "./Editor";
import ListManager from "../ItemList/ItemList";

class AbilityList extends ListManager {

	render() {
		const {list, allowEditing} = this.state;
		let addButton              = this.buttonEditOrNothing("Add Ability", <AbilityEditor
				save={this.addToList.bind(this)}/>);

		return (
				<div id="AbilityList">
					<h4>Abilities</h4>
					{addButton}
					{list.map((item, index) => (
							<AbilityDescription key={item._id}
							                    item={item}
							                    save={this.updateItem.bind(this)}
							                    remove={this.removeItem.bind(this)}
							                    allowEditing={allowEditing}/>))}
				</div>
		);
	}
}

export default AbilityList;
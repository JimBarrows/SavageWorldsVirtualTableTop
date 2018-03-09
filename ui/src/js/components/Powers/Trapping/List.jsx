import React from "react";
import ListManager from "../../Item/List";
import TrappingEditor from "./Editor";
import TrappingDescription from "./Description";


class TrappingList extends ListManager {

	render() {
		let {list, allowEditing}    = this.state;
		let addButton               = this.buttonEditOrNothing("Add Arcane Background", <TrappingEditor
				save={this.addToList.bind(this)}/>);
		return (
				<div id="trappingList">
					<h2>Trappings & Effects</h2>
					{addButton}
					<dl>
						{list.map((item, index) => <TrappingDescription key={item._id}
						                                                item={item}
						                                                save={this.updateItem.bind(this)}
						                                                remove={this.removeItem.bind(this)}
						                                                allowEditing={allowEditing}/>)}
					</dl>
				</div>
		);
	}
}

export default TrappingList;
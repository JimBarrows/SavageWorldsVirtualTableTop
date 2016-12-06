import React from "react";
import ListManager from "../ItemList/ItemList";
import EdgeTypeEditor from "./EdgeTypeEditor";
import EdgeTypeDescription from "./EdgeTypeDescription";

class EdgeTypeList extends ListManager {

	render() {
		let {list, allowEditing}    = this.state;
		let addButton               = this.buttonEditOrNothing("Add Edge Type", <EdgeTypeEditor
				save={this.addToList.bind(this)}/>);
		return (
				<div id="edgeTypeList">
					<h2>Edge Types</h2>
					{addButton}
					{list.map((item, index) => <EdgeTypeDescription key={item._id}
					                                                item={item}
					                                                save={this.updateItem.bind(this)}
					                                                remove={this.removeItem.bind(this)}
					                                                allowEditing={allowEditing}/>)}
				</div>
		);
	}
}

export default EdgeTypeList;
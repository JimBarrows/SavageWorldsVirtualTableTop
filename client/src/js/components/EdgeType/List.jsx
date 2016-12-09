import React from "react";
import ListManager from "../Item/List";
import EdgeTypeEditor from "./Editor";
import EdgeTypeDescription from "./Description";

class EdgeTypeList extends ListManager {

	render() {
		let {list, allowEditing}    = this.state;
		let addButton               = this.buttonEditOrNothing("Add Edge Type", <EdgeTypeEditor
				save={this.addToList.bind(this)}/>);
		return (
				<div id="edgeTypeList">
					<h1>Edge Types</h1>
					{addButton}
					<dl>
						{list.map((item, index) => <EdgeTypeDescription key={item._id}
						                                                item={item}
						                                                save={this.updateItem.bind(this)}
						                                                remove={this.removeItem.bind(this)}
						                                                allowEditing={allowEditing}/>)}
					</dl>
				</div>
		);
	}
}

export default EdgeTypeList;
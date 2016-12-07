import React from "react";
import ListManager from "../Item/List";
import EdgeEditor from "./Editor";
import EdgeDescription from "./Description";


class EdgeList extends ListManager {

	render() {
		let {list, allowEditing}    = this.state;
		let addButton               = this.buttonEditOrNothing("Add Edge", <EdgeEditor
				save={this.addToList.bind(this)}/>);
		return (
				<div id="edgeList">
					<h2>Edges</h2>
					{addButton}
					{list.map((item, index) => <EdgeDescription key={item._id}
					                                            item={item}
					                                            save={this.updateItem.bind(this)}
					                                            remove={this.removeItem.bind(this)}
					                                            allowEditing={allowEditing}/>)}
				</div>
		);
	}
}

export default EdgeList;
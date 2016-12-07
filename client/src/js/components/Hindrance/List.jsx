import React from "react";
import ListManager from "../Item/List";
import HindranceEditor from "./Editor";
import HindranceDescription from "./Description";

class HindranceList extends ListManager {

	render() {
		let {list, allowEditing}    = this.state;
		let addButton               = this.buttonEditOrNothing("Add Hindrance", <HindranceEditor
				save={this.addToList.bind(this)}/>);
		return (
				<div id="edgeTypeList">
					<h2>Hindrances</h2>
					{addButton}
					{list.map((item, index) => <HindranceDescription key={item._id}
					                                                 item={item}
					                                                 save={this.updateItem.bind(this)}
					                                                 remove={this.removeItem.bind(this)}
					                                                 allowEditing={allowEditing}/>)}
				</div>
		);
	}
}

export default HindranceList;
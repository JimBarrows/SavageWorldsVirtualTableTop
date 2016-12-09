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
					<h1>Hindrances</h1>
					{addButton}
					<dl>
						{list.map((item, index) => <HindranceDescription key={item._id}
						                                                 item={item}
						                                                 save={this.updateItem.bind(this)}
						                                                 remove={this.removeItem.bind(this)}
						                                                 allowEditing={allowEditing}/>)}
					</dl>
				</div>
		);
	}
}

export default HindranceList;
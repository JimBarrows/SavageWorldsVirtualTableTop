import React from "react";
import ListManager from "../../Item/List";
import PowerEditor from "./Editor";
import PowerDescription from "./Description";


class PowerList extends ListManager {

	render() {
		let {list, allowEditing}    = this.state;
		let addButton               = this.buttonEditOrNothing("Add Power", <PowerEditor
				save={this.addToList.bind(this)}/>);
		return (
				<div id="powerList">
					<h2>Powers</h2>
					{addButton}
					<dl>
						{list.map((item, index) => <PowerDescription key={item._id}
						                                             item={item}
						                                             save={this.updateItem.bind(this)}
						                                             remove={this.removeItem.bind(this)}
						                                             allowEditing={allowEditing}/>)}
					</dl>
				</div>
		);
	}
}

export default PowerList;
import React from "react";
import ListManager from "../../Item/List";
import ArcaneBackgroundEditor from "./Editor";
import ArcaneBackgroundDescription from "./Description";


class ArcaneBackgroundList extends ListManager {

	render() {
		let {list, allowEditing}    = this.state;
		let addButton               = this.buttonEditOrNothing("Add Arcane Background", <ArcaneBackgroundEditor
				save={this.addToList.bind(this)}/>);
		return (
				<div id="arcaneList">
					<h2>Arcane Backgrounds</h2>
					{addButton}
					<dl>
						{list.map((item, index) => <ArcaneBackgroundDescription key={item._id}
						                                                        item={item}
						                                                        save={this.updateItem.bind(this)}
						                                                        remove={this.removeItem.bind(this)}
						                                                        allowEditing={allowEditing}/>)}
					</dl>
				</div>
		);
	}
}

export default ArcaneBackgroundList;
import React from "react";
import RaceDescription from "./Description";
import RaceEditor from "./Editor";
import ListManager from "../Item/List";

class RaceList extends ListManager {

	render() {
		let {list, allowEditing}       = this.state;
		let addButton = this.buttonEditOrNothing("Add Race", <RaceEditor
				save={this.addToList.bind(this)}/>);

		return (
				<div id="raceList">
					<h1>Races</h1>
					{addButton}
					<dl>
						{list.map((item, index) => <RaceDescription
								key={item._id}
								item={item}
								save={this.updateItem.bind(this)}
								remove={this.removeItem.bind(this)}
								allowEditing={allowEditing}/>
						)}
					</dl>
				</div>
		);
	}

}

export default RaceList;
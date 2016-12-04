import React from "react";
import RaceDescription from "./RaceDescription";
import RaceEditor from "../components/RaceEditor";
import ListManager from "./ListManager";


class RaceList extends ListManager {

	render() {
		let {list}       = this.state;
		let addButton    = this.buttonEditOrNothing("Add Race", <RaceEditor save={this.addToList.bind(this)}/>);

		return (
				<div id="races">
					<h2>Races</h2>
					{addButton}
					{list.map((item, index) => <RaceDescription
							key={item._id || `${item.name.replace(" ", "_")}_${index}`}
							item={item} index={index}
							save={this.updateItem.bind(this)}
							remove={this.removeItem.bind(this)}/>
					)}
				</div>
		);
	}

}

export default RaceList;
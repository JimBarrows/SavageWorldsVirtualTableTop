import React from "react";
import ObjectId from "bson-objectid";
import {ItemList} from "../../Item";
import Vehicle from "./Item";
import {ListTablePanel} from "bootstrap-react-components";

class VehicleList extends ItemList {

	propsToState(props) {
		let {list, adding, allowEditing}  = props;
		this.setState({
			list,
			adding,
			allowEditing
		});
	}

	render() {
		let {list, allowEditing, adding}    = this.state;

		return (
				<div id="vehicleList">
					<h2>Vehicles</h2>

					<ListTablePanel title="Vehicles" id="vehicle" onAddClick={this.add.bind(this)}>
						<thead>
						<tr>
							<th>Era</th>
							<th>Type</th>
							<th>Mode</th>
							<th>Name</th>
							<th>Acceleration</th>
							<th>Top Speed</th>
							<th>Toughness</th>
							<th>AP</th>
							<th>Crew</th>
							<th>Passengers</th>
							<th>Cost</th>
							<th>Notes</th>
							<th>Examples</th>
							<th>Weapons</th>
							<th>Climb</th>
						</tr>
						</thead>
						<tbody>
						{ adding ? <Vehicle allowEditing={true}
						                    editing={true}
						                    item={this.newItem()}
						                    key="New Row"
						                    save={this.addToList.bind(this)}/> : null}
						{list.map((item, index) => <Vehicle allowEditing={allowEditing}
						                                    key={item._id}
						                                    item={item}
						                                    save={this.updateItem.bind(this)}
						                                    remove={this.removeItem.bind(this)}/>)}
						</tbody>
					</ListTablePanel>
				</div>
		);
	}


	newItem() {
		return {
			_id: ObjectId.generate(),
			era: "",
			type: "",
			mode: "",
			name: "",
			acceleration: 0,
			topSpeed: 0,
			toughness: 0,
			armorPoints: 0,
			crew: 0,
			passengers: 0,
			cost: 0,
			notes: "",
			examples: "",
			weapons: "",
			climb: 0
		}
	}

}

export default VehicleList;


import React from "react";
import ObjectId from "bson-objectid";
import {ItemList} from "../../../Item";
import VehicleMountedAndAtGun from "./Item";
import {ListTablePanel} from "bootstrap-react-components";

class VehicleMountedAndAtGunList extends ItemList {

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
				<div id="vehicleMountedAndAtGunList">
					<h3>Vehicle Mounted and AT Guns</h3>

					<ListTablePanel title="Vehicle Mounted and AT Guns" id="vehicleMountedAndAtGun"
					                onAddClick={this.add.bind(this)}>
						<thead>
						<tr>
							<th>Name</th>
							<th>Range</th>
							<th>AP Rounds</th>
							<th>HE Rounds</th>
							<th>RoF</th>
						</tr>
						</thead>
						<tbody>
						{ adding ? <VehicleMountedAndAtGun allowEditing={true}
						                                   editing={true}
						                                   item={this.newItem()}
						                                   key="New Row"
						                                   save={this.addToList.bind(this)}/> : null}
						{list.map((item, index) => <VehicleMountedAndAtGun allowEditing={allowEditing}
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
			name: "",
			range: {short: 0, mediume: 0, long: 0},
			apRoundDamage: {
				attribute: "",
				diceCount: 0,
				bonus: 0,
				dice: "d4"
			},
			heRoundDamage: {
				attribute: "",
				diceCount: 0,
				bonus: 0,
				dice: "d4"
			},
			rateOfFire: 0
		}
	}

}

export default VehicleMountedAndAtGunList;


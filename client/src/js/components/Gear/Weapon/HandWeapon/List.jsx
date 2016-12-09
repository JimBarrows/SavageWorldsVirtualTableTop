import React from "react";
import ObjectId from "bson-objectid";
import {ItemList} from "../../../Item";
import HandWeapon from "./Item";
import {ListTablePanel} from "bootstrap-react-components";

class HandWeaponList extends ItemList {

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
				<div id="handWeaponList">
					<h3>Hand Weapons</h3>

					<ListTablePanel title="Hand Weapons" id="handWeapon" onAddClick={this.add.bind(this)}>
						<thead>
						<tr>
							<th>Name</th>
							<th>Cost</th>
							<th>Type</th>
							<th>Weight</th>
							<th>Damage</th>
							<th>Era</th>
							<th>Notes</th>
						</tr>
						</thead>
						<tbody>
						{ adding ? <HandWeapon allowEditing={true}
						                       editing={true}
						                       item={this.newItem()}
						                       key="New Row"
						                       save={this.addToList.bind(this)}/> : null}
						{list.map((item, index) => <HandWeapon allowEditing={allowEditing}
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
			cost: 0,
			damage: {
				attribute: "",
				diceCount: 0,
				bonus: 0,
				dice: "d4"
			},
			era: "",
			notes: "",
			type: "",
			weight: 0
		}
	}

}

export default HandWeaponList;


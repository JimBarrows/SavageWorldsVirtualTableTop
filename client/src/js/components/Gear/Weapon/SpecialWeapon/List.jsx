import React from "react";
import ObjectId from "bson-objectid";
import {ItemList} from "../../../Item";
import SpecialWeapon from "./Item";
import {ListTablePanel} from "bootstrap-react-components";

class SpecialWeaponList extends ItemList {

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
				<div id="specialWeaponList">
					<h3>Special Weapons</h3>

					<ListTablePanel title="Special Weapons" id="specialWeapon" onAddClick={this.add.bind(this)}>
						<thead>
						<tr>
							<th>Name</th>
							<th>Weight</th>
							<th>AP</th>
							<th>Burst Template</th>
							<th>Damage</th>
							<th>Min. Str.</th>
							<th>Range</th>
							<th>ROF</th>
							<th>Shots</th>
						</tr>
						</thead>
						<tbody>
						{ adding ? <SpecialWeapon allowEditing={true}
						                          editing={true}
						                          item={this.newItem()}
						                          key="New Row"
						                          save={this.addToList.bind(this)}/> : null}
						{list.map((item, index) => <SpecialWeapon allowEditing={allowEditing}
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
			minStr: "",
			notes: "",
			range: {
				short: 0,
				medium: 0,
				long: 0
			},
			rateOfFire: 1,
			shots: 1,
			type: "",
			weight: 0
		}
	}

}

export default SpecialWeaponList;


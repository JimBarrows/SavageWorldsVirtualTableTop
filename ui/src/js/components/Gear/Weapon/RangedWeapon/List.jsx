import React from "react";
import ObjectId from "bson-objectid";
import {ItemList} from "../../../Item";
import RangedWeapon from "./Item";
import {ListTablePanel} from "bootstrap-react-components";

class RangedWeaponList extends ItemList {

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
				<div id="rangedWeaponList">
					<h3>Ranged Weapons</h3>

					<ListTablePanel title="Ranged Weapons" id="rangedWeapon" onAddClick={this.add.bind(this)}>
						<thead>
						<tr>
							<th>Name</th>
							<th>Cost</th>
							<th>Type</th>
							<th>Weight</th>
							<th>Damage</th>
							<th>Era</th>
							<th>Notes</th>
							<th>Min. Str.</th>
							<th>Range</th>
							<th>ROF</th>
							<th>Shots</th>
						</tr>
						</thead>
						<tbody>
						{ adding ? <RangedWeapon allowEditing={true}
						                         editing={true}
						                         item={this.newItem()}
						                         key="New Row"
						                         save={this.addToList.bind(this)}/> : null}
						{list.map((item, index) => <RangedWeapon allowEditing={allowEditing}
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

export default RangedWeaponList;


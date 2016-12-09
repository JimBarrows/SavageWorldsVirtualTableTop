import React from "react";
import ObjectId from "bson-objectid";
import {ItemList} from "../../Item";
import Armor from "./Item";
import {ListTablePanel} from "bootstrap-react-components";

class ArmorList extends ItemList {

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
				<div id="armorList">
					<h2>Armor</h2>

					<ListTablePanel title="Armor" id="armor" onAddClick={this.add.bind(this)}>
						<thead>
						<tr>
							<th>Name</th>
							<th>Cost</th>
							<th>Era</th>
							<th>Armor</th>
							<th>Type</th>
							<th>Weight</th>
						</tr>
						</thead>
						<tbody>
						{ adding ? <Armor allowEditing={true}
						                  editing={true}
						                  item={this.newItem()}
						                  key="New Row"
						                  save={this.addToList.bind(this)}/> : null}
						{list.map((item, index) => <Armor allowEditing={allowEditing}
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
			era: "",
			armor: 0,
			armorVsBullets: 0,
			armorProtection: 0,
			type: "",
			weight: 0
		}
	}

}

export default ArmorList;


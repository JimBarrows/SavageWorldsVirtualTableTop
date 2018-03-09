import React from "react";
import ObjectId from "bson-objectid";
import {ItemList} from "../../../Item";
import Ammunition from "./Item";
import {ListTablePanel} from "bootstrap-react-components";

class AmmunitionList extends ItemList {

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
				<div id="ammunitionList">
					<h3>Ammunition</h3>

					<ListTablePanel title="Ammunition" id="ammunition" onAddClick={this.add.bind(this)}>
						<thead>
						<tr>
							<th>Name</th>
							<th>Cost</th>
							<th>Notes</th>
							<th>Weight</th>
						</tr>
						</thead>
						<tbody>
						{ adding ? <Ammunition allowEditing={true}
						                       editing={true}
						                       item={this.newItem()}
						                       key="New Row"
						                       save={this.addToList.bind(this)}/> : null}
						{list.map((item, index) => <Ammunition allowEditing={allowEditing}
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
			type: "",
			weight: 0
		}
	}

}

export default AmmunitionList;


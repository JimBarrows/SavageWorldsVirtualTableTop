import React from 'react';
import ObjectId from 'bson-objectid';
import {ItemList} from '../../Item';
import MundaneItem from './Item';
import {ListTablePanel} from 'bootstrap-react-components';

class MundaneItemList extends ItemList {

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
				<div id='mundaneItemList'>
					<h3>Mundane Items</h3>

					<ListTablePanel title='Mundane Items' id='mundaneItem' onAddClick={this.add.bind(this)}>
						<thead>
						<tr>
							<th>Name</th>
							<th>Cost</th>
							<th>Type</th>
							<th>Weight</th>
						</tr>
						</thead>
						<tbody>
						{ adding ? <MundaneItem allowEditing={true}
						                        editing={true}
						                        item={this.newItem()}
						                        key='New Row'
						                        save={this.addToList.bind(this)}/> : null}
						{list.map((item, index) => <MundaneItem allowEditing={allowEditing}
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
			name: '',
			cost: 0,
			type: '',
			weight: 0
		}
	}

}

export default MundaneItemList;


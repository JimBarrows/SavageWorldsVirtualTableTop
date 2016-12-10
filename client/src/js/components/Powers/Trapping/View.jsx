import {RowControlButtons} from "bootstrap-react-components";
import React from "react";

class TrappingView extends React.Component {

	render() {
		let {_id, name, description, type, edit, remove, save, allowEditing} = this.props;
		return (
				<div class="arcaneViewPage">
					<h3>{type} - {name}{allowEditing ? <RowControlButtons id={_id}
					                                                      editing={false}
					                                                      edit={edit}
					                                                      save={save}
					                                                      remove={remove}/> : ""}
					</h3>
					{description}
				</div>
		);
	}
}

export default TrappingView;
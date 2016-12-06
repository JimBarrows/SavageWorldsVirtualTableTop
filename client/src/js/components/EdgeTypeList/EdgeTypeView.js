import {RowControlButtons} from "bootstrap-react-components";
import React from "react";

class EdgeTypeView extends React.Component {

	render() {

		let {_id, name, description, edit, remove, save, allowEditing} = this.props;

		return (
				<div class="EdgeTypeView">
					<h3>
						{name}
						{allowEditing ? <RowControlButtons id={_id}
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

export default EdgeTypeView;
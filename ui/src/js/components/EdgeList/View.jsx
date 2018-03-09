import {RowControlButtons} from "bootstrap-react-components";
import React from "react";

class EdgeView extends React.Component {

	render() {
		let {_id, name, description, edgeType, edit, remove, save, allowEditing} = this.props;
		return (
				<div class="edgeViewPage">
					<dt>
						{name} {edgeType ? " - " + edgeType.name : ""}
						{allowEditing ? <RowControlButtons id={_id}
						                                   editing={false}
						                                   edit={edit}
						                                   save={save}
						                                   remove={remove}/> : ""}
					</dt>
					<dd>{description}</dd>
				</div>
		);
	}
}

export default EdgeView;
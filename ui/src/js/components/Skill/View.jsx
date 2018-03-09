import {RowControlButtons} from "bootstrap-react-components";
import React from "react";

class Viewer extends React.Component {

	render() {
		let {_id, name, attribute, description, edit, remove, save, allowEditing} = this.props;
		return (
				<div id="ViewerPage">
					<dt>{name}
						<small>({attribute})</small>
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

export default Viewer;
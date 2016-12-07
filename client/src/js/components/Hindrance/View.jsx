import {RowControlButtons} from "bootstrap-react-components";
import React from "react";

class HindranceView extends React.Component {

	render() {

		let {_id, name, description, severity, edit, remove, save, allowEditing} = this.props;

		return (
				<div class="HindranceView">
					<h3>
						{name}
						<small>({severity})</small>
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

export default HindranceView;
import React from "react";
import {RowControlButtons} from "bootstrap-react-components";

class AbilityView extends React.Component {

	render() {

		let {_id, name, cost, description, edit, remove, save, allowEditing} = this.props;

		return (
				<div class="AbilityViewPage">
					<h5>
						{name}
						<small>
							<bold>Cost:</bold>
							{cost}
						</small>
						{allowEditing ? <RowControlButtons id={_id}
						                                   editing={false}
						                                   edit={edit}
						                                   save={save}
						                                   remove={remove}/> : ""}
					</h5>
					{description}
				</div>
		);
	}
}

export default AbilityView;
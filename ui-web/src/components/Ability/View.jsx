import React from "react";
import {RowControlButtons} from "bootstrap-react-components";

class AbilityView extends React.Component {

	render() {

		let {_id, name, cost, description, edit, remove, save, allowEditing} = this.props;

		return (
				<div class="AbilityViewPage">
					<dt>
						{name}

						{allowEditing ? <RowControlButtons id={_id}
						                                   editing={false}
						                                   edit={edit}
						                                   save={save}
						                                   remove={remove}/> : ""}
					</dt>
					<dd>
						<small>
							<bold>Cost:</bold>
							{cost}
						</small>
						<br/>
						{description}</dd>
				</div>
		);
	}
}

export default AbilityView;
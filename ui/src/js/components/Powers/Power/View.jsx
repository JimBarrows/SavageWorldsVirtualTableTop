import {RowControlButtons} from "bootstrap-react-components";
import React from "react";

class PowerView extends React.Component {

	render() {
		let {_id, name, description, level, duration, maintenance, powerPoints, range, trappings, edit, remove, save, allowEditing} = this.props;
		let rangeDisplay                                                                                                            = `${range.short} / ${range.medium} / ${range.long}`;
		if (range.short === 0) {
			rangeDisplay = "Touch";
		} else if ((range.short === range.medium) && ( range.mediume === range.long)) {
			rangeDisplay = range.short
		}
		let durationDisplay = duration + ( maintenance > 0 ? `(${maintenance}/round)` : "");
		return (
				<div class="powerViewPage">
					<h3>{name}{allowEditing ? <RowControlButtons id={_id}
					                                             editing={false}
					                                             edit={edit}
					                                             save={save}
					                                             remove={remove}/> : ""}
					</h3>
					<p><b>Rank: </b>{level} <b>Power Points: </b>{powerPoints} <b>Range: </b>{rangeDisplay}
						<b>Duration: </b>{durationDisplay}</p>
					<p><b>Trappings: </b> {trappings}</p>
					{description}
				</div>
		);
	}
}

export default PowerView;
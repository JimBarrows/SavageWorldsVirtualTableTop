'use strict';
import React from "react";
import {withRouter} from "react-router";
import * as Action from "../actions/PlotPointActions";
import {ListTablePanel, RowControlButtons} from "bootstrap-react-components";


class PlotPointTable extends React.Component {

	add() {
		Action.newPlotPoint();
	}

	constructor(props) {
		super(props);
	}

	edit(id) {
		Action.editPlotPoint(id);
	}

	remove(plotPoint) {
		Action.remove(plotPoint);
	}

	render() {
		let {plotPoints} = this.props;

		const Rows = plotPoints.map((plotPoint, index) => {
			return <tr key={index}>
				<td>{plotPoint.name}</td>
				<td>
					<RowControlButtons id={plotPoint.name} edit={this.edit.bind(this, plotPoint._id)}
					                   remove={this.remove.bind(this, plotPoint)}/>
				</td>
			</tr>
		});
		return (
				<ListTablePanel id="plotPointTable" title="Plot Points" onAddClick={this.add.bind(this)}>
					<thead>
					<tr>
						<th>Name</th>
					</tr>
					</thead>
					<tbody>
					{Rows}
					</tbody>
				</ListTablePanel>
		);
	}
}
export default withRouter(PlotPointTable);
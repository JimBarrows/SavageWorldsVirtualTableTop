import React from "react";
import {ListTablePanel, RowControlButtons} from "bootstrap-react-components";
import {connect} from "react-redux";
import {push} from "react-router-redux";
import {load, remove} from "../actions/PlotPointActions";

class PlotPointListPanelPanel extends React.Component {

	create() {
		this.props.create();
	}

	componentWillMount() {
		this.props.load();
	}
	constructor(props) {
		super(props);
	}

	update(id) {
		this.props.update(id);
	}

	remove(plotPoint) {
		this.props.remove(plotPoint);
	}

	render() {
		let {plotPoints} = this.props;

		const Rows = plotPoints.map((plotPoint, index) => {
			return <tr key={index}>
				<td>{plotPoint.name}</td>
				<td>
					<RowControlButtons id={plotPoint.name} edit={this.update.bind(this, plotPoint._id)}
					                   remove={this.remove.bind(this, plotPoint)}/>
				</td>
			</tr>
		});
		return (
				<ListTablePanel id="plotPointTable" title="Plot Points" onAddClick={this.create.bind(this)}>
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

const mapStateToProps = (state) => {
	return {
		plotPoints: state.PlotPoints.plotPoints
	};
};

const mapDispatchToProps = (dispatch) => {
	return {
		create: () => dispatch(push("/plotPoints/add")),
		update: (plotPointId) => dispatch(push(`/plotPoint/${plotPointId}/edit`)),
		load: () => dispatch(load()),
		remove: (plotPoint) => dispatch(remove(plotPoint))
	};
};

export default connect(mapStateToProps, mapDispatchToProps)(PlotPointListPanelPanel);
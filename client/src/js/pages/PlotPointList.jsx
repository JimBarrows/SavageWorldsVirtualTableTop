import React from "react";
import {withRouter} from "react-router";
import {ListTablePanel, RowControlButtons} from "bootstrap-react-components";
import {connect} from "react-redux";


class PlotPointListPanelPanel extends React.Component {

	add() {

	}

	constructor(props) {
		super(props);
	}

	edit(id) {

	}

	remove(plotPoint) {

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
const mapStateToProps = (state) => {
	return {
		plotPoints: state.deckList ? state.deckList : []
	};
};

const mapDispatchToProps = (dispatch) => {
	return {
		add: (deck) => {
			dispatch(deckAdd(deck));
		}
	};
};

export default connect(mapStateToProps, mapDispatchToProps)(withRouter(PlotPointListPanelPanel));
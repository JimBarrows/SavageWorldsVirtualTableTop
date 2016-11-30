import {PageHeader} from "bootstrap-react-components";
import React from "react";
import {connect} from "react-redux";
import {create, newPlotPoint} from "../actions/PlotPointActions";
import PlotPointForm from "../components/PlotPointForm";
import {push} from "react-router-redux";

class AddPlotPoint extends React.Component {

	componentWillMount() {
		this.props.newPlotPoint();
	}

	componentWillReceiveProps(nextProps) {
		this.setState({
			plotPoint: nextProps.plotPoint
		});
	}

	constructor(props) {
		super(props);
		this.state = {
			plotPoint: {}
		}
	}

	render() {
		return (
				<div id="AddPlotPointPage">
					<PageHeader id="AddPlotPoint">
						<h1>Add Plot Point</h1>
					</PageHeader>
					<PlotPointForm plotPoint={this.state.plotPoint}
					               onSubmit={this.props.create.bind(this)}
					               onCancel={this.props.onCancel.bind(this)}/>
				</div>
		);
	}
}

const mapStateToProps = (state) => {
	return {
		plotPoint: state.PlotPoint.plotPoint
	};
};

const mapDispatchToProps = (dispatch) => {
	return {
		create: (plotPoint) => dispatch(create(plotPoint)),
		newPlotPoint: () => dispatch(newPlotPoint()),
		onCancel: () => dispatch(push("/"))
	};
};

export default connect(mapStateToProps, mapDispatchToProps)(AddPlotPoint);
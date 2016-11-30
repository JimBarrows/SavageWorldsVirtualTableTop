import {PageHeader} from "bootstrap-react-components";
import React from "react";
import {connect} from "react-redux";
import {loadPlotPoint, update} from "../actions/PlotPointActions";
import PlotPointForm from "../components/PlotPointForm";

class UpdatePlotPoint extends React.Component {

	componentWillMount() {
		this.props.load(this.props.params.plotPointId);
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
				<div id="UpdatePlotPointPage">
					<PageHeader id="UpdatePlotPoint">
						<h1>Update Plot Point</h1>
					</PageHeader>
					<PlotPointForm plotPoint={this.state.plotPoint}
					               onSubmit={this.props.save.bind(this)}/>

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
		load: (id) => dispatch(loadPlotPoint(id)),
		save: (plotPoint) => dispatch(update(plotPoint))
	};
};

export default connect(mapStateToProps, mapDispatchToProps)(UpdatePlotPoint);
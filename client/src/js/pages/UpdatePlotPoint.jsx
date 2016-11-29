import {PageHeader} from "bootstrap-react-components";
import React from "react";
import {connect} from "react-redux";
import {loadPlotPoint, update} from "../actions/PlotPointActions";
import PlotPointForm from "../components/PlotPointForm";

class UpdatePlotPoint extends React.Component {

	componentWillMount() {
		this.props.load(this.props.params.plotPointId);
		let {name, description, _id}  = this.props.plotPoint;
		this.setState({
			name, description, _id
		});
	}

	componentWillReceiveProps(nextProps) {
		let {name, description, _id}  = nextProps.plotPoint;
		this.setState({
			name, description, _id
		});
	}

	render() {
		let {name, description, _id} = this.state;
		return (
				<div id="UpdatePlotPointPage">
					<PageHeader id="UpdatePlotPoint">
						<h1>Update Plot Point</h1>
					</PageHeader>
					<PlotPointForm _id={_id}
					               name={name}
					               description={description}
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
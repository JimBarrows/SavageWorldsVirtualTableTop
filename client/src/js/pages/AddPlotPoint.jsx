import {PageHeader} from "bootstrap-react-components";
import React from "react";
import {connect} from "react-redux";
import {create, newPlotPoint} from "../actions/PlotPointActions";
import PlotPointForm from "../components/PlotPointForm";

class AddPlotPoint extends React.Component {

	componentWillMount() {
		this.props.newPlotPoint();
		this.setState({
			name: "",
			description: ""
		})
	}

	componentWillReceiveProps(nextProps) {
		let {name, description, _id}  = nextProps.plotPoint;
		this.setState({
			name, description, _id
		});
	}

	onSubmit(e) {
		e.preventDefault();
		let {name, description} = this.state;
		this.props.create({
			name,
			description
		})
	}

	render() {
		let {name, description} = this.state;
		return (
				<div id="AddPlotPointPage">
					<PageHeader id="AddPlotPoint">
						<h1>Add Plot Point</h1>
					</PageHeader>
					<PlotPointForm name={name} description={description} onSubmit={this.props.create.bind(this)}/>
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
		newPlotPoint: () => dispatch(newPlotPoint())
	};
};

export default connect(mapStateToProps, mapDispatchToProps)(AddPlotPoint);
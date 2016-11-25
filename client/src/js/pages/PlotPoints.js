'use strict';
import React from "react";
import {withRouter} from "react-router";
import {PageHeader} from "bootstrap-react-components";
import PlotPointStore from "../stores/PlotPointStore";
import {PlotPointEvent} from "../constants";
import * as Action from "../actions/PlotPointActions";
import PlotPointTable from "../components/PlotPointListPanel";


class PlotPoints extends React.Component {

	componentWillMount() {
		PlotPointStore.on(PlotPointEvent.EDITABLE, this.showEdit);
		PlotPointStore.on(PlotPointEvent.LOAD_SUCCESS, this.reload);
		PlotPointStore.on(PlotPointEvent.NEW, this.showAdd);
		PlotPointStore.on(PlotPointEvent.REMOVE_SUCCESS, this.reload);
		Action.load();
	}

	componentWillUnmount() {
		PlotPointStore.removeListener(PlotPointEvent.EDITABLE, this.showEdit);
		PlotPointStore.removeListener(PlotPointEvent.LOAD_SUCCESS, this.reload);
		PlotPointStore.removeListener(PlotPointEvent.NEW, this.showAdd);
		PlotPointStore.removeListener(PlotPointEvent.REMOVE_SUCCESS, this.reload);
	}

	constructor() {
		super();
		this.state    = {
			plotPoints: []
			, error: null
		};
		this.showEdit = this.showEdit.bind(this);
		this.reload   = this.reload.bind(this);
		this.showAdd  = this.showAdd.bind(this);
	}

	showEdit() {
		this.props.router.push(`/plotPoint/edit`)
	}

	render() {
		let plotPoints = this.state.plotPoints;
		return (
				<div>
					<PageHeader>
						<h1>Plot Points</h1>
					</PageHeader>
					<PlotPointTable plotPoints={plotPoints}/>
				</div>
		);
	}

	showAdd() {
		this.props.router.push('/plotPoint/add');
	}

	reload() {
		this.setState({
			plotPoints: PlotPointStore.all
		})
	};

}
export default withRouter(PlotPoints);
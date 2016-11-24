'use strict';
import React from "react";
import {withRouter} from "react-router";
import {PageHeader} from "bootstrap-react-components";
import * as PlotPointAction from "../actions/PlotPointActions";
import {PlotPointEvent} from "../constants";
import PlotPointForm from "../components/PlotPointForm";
import PlotPointStore from "../stores/PlotPointStore";

class PlotPointEdit extends React.Component {

	constructor() {
		super();
		this.updated = this.updated.bind(this);
	}

	componentWillMount() {
		PlotPointStore.on(PlotPointEvent.SAVE_SUCCESS, this.updated)
	}

	componentWillUnmount() {
		PlotPointStore.removeListener(PlotPointEvent.SAVE_SUCCESS, this.updated)
	}

	updated() {
		this.props.router.push('/');
	}

	render() {
		return (
				<div>
					<PageHeader>
						<h1>Edit Plot Point</h1>
					</PageHeader>
					<PlotPointForm onSubmit={PlotPointAction.update}/>
				</div>
		);
	}
}
export default withRouter(PlotPointEdit);
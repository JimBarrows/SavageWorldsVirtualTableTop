'use strict';

import {PageHeader} from "bootstrap-react-components";
import * as PlotPointAction from "../actions/PlotPointActions";
import {PlotPointEvent} from "../constants";
import PlotPointForm from "../components/PlotPointForm";
import PlotPointStore from "../stores/PlotPointStore";
import React from "react";
import {withRouter} from "react-router";

class PlotPointAdd extends React.Component {

	componentWillMount() {
		PlotPointStore.on(PlotPointEvent.SAVE_SUCCESS, this.created)
	}

	componentWillUnmount() {
		PlotPointStore.removeListener(PlotPointEvent.SAVE_SUCCESS, this.created)
	}

	constructor() {
		super();
		this.created = this.created.bind(this);
	}

	created() {
		this.props.router.push('/');
	}

	render() {
		return (
				<div>
					<PageHeader>
						<h1>Add Plot Point</h1>
					</PageHeader>
					<PlotPointForm plotPoint={PlotPointStore.current} onSubmit={PlotPointAction.create}/>
				</div>
		);
	}
}

export default withRouter(PlotPointAdd);
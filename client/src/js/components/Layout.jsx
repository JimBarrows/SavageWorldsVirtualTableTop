import React from "react";
import {MESSAGE_CONTEXT} from "../constants";

class Layout extends React.Component {

	componentWillReceiveProps(nextProps) {
		this.setState({
			children: nextProps.children
		});
	}

	render() {
		let {app}        =this.props;
		let message      = "";
		let contextClass = "alert-info";
		switch (app.message.context) {
			case MESSAGE_CONTEXT.danger:
				contextClass = "alert-danger";
				break;
			case MESSAGE_CONTEXT.info:
				contextClass = "alert-info";
				break;
			case MESSAGE_CONTEXT.success:
				contextClass = "alert-success";
				break;
			case MESSAGE_CONTEXT.warning:
				contextClass = "alert-warning";
				break;
			default:
				contextClass = "alert-info";
		}
		if (app.message.show) {
			message = <div class={`alert ${contextClass}`} role="alert">{app.message.message}</div>
		}

		return (
				<div id="layout">
					{message}
					{this.props.children}
				</div>
		);
	}
}

export default Layout;
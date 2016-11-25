import Footer from "./Footer";
import Header from "./Header";
import {MESSAGE_CONTEXT} from "../constants";
import React from "react";
import {withRouter} from "react-router";

class Layout extends React.Component {

	constructor() {
		super();
		this.state = {
			title: "Savage World Virtual Table Top"
		};
	}

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

		const containerStyle = {
			marginTop: "60px"
		};
		const {location}     = this.props;

		return (
				<div id="layout" class="container theme-showcase" role="main" style={containerStyle}>
					<Header location={location} title={this.state.title}/>
					{message}
					{this.props.children}
					<Footer/>
				</div>
		);
	}
}

export default withRouter(Layout);
import Footer from "./Footer";
import Header from "./Header";
import constants from "../constants";
import React from "react";
import {connect} from "react-redux";
import {logout} from "../actions";

let {MESSAGE_CONTEXT_DANGER, MESSAGE_CONTEXT_INFO, MESSAGE_CONTEXT_SUCCESS, MESSAGE_CONTEXT_WARNING} = constants;
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
		let {app, logout}        =this.props;
		let message              = "";
		let contextClass         = "alert-info";
		switch (app.message.context) {
			case MESSAGE_CONTEXT_DANGER:
				contextClass = "alert-danger";
				break;
			case MESSAGE_CONTEXT_INFO:
				contextClass = "alert-info";
				break;
			case MESSAGE_CONTEXT_SUCCESS:
				contextClass = "alert-success";
				break;
			case MESSAGE_CONTEXT_WARNING:
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
		const {location, user}     = this.props;
		return (
				<div id="layout" class="container theme-showcase" role="main" style={containerStyle}>
					<Header location={location} title={this.state.title} user={user} logout={logout.bind(this)}/>
					{message}
					{this.props.children}
					<Footer/>
				</div>
		);
	}
}

const mapStateToProps = (state, ownProps) => {
	return {
		app: state.app,
		location: ownProps.location,
		user: state.user
	};
};

const mapDispatchToProps = (dispatch) => {
	return {
		logout: () => {
			dispatch(logout());
		}
	};
};

export default connect(mapStateToProps, mapDispatchToProps)(Layout);
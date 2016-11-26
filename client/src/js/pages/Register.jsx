import {PageHeader} from "bootstrap-react-components";
import React from "react";
import {connect} from "react-redux";

class Register extends React.Component {

	render() {
		return (
				<div id="RegisterPage">
					<PageHeader id="Register">
						<h1>Register</h1>
					</PageHeader>
				</div>
		);
	}
}

const mapStateToProps = (state) => {
	return {};
};

const mapDispatchToProps = (dispatch) => {
	return {};
};

export default connect(mapStateToProps, mapDispatchToProps)(Register);
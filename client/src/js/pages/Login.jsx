import {EmailFormGroup, PasswordFormGroup} from "bootstrap-react-components";
import React from "react";
import {connect} from "react-redux";
import {userLogin} from "../actions";

class Login extends React.Component {

	constructor() {
		super();
		this.state = {
			username: "",
			password: "",
			usernameError: "",
			passwordError: ""
		}
	}

	onSubmit(e) {
		e.preventDefault();
		let nextPage = this.props.location.query && this.props.location.query.next ? this.props.location.query.next : "/";
		this.props.login(this.state.username, this.state.password, nextPage);
	}

	render() {
		let {username, password, usernameError, passwordError} = this.state;
		return (
				<div id="LoginPage">
					<form id="loginForm" class="form-signin" onSubmit={this.onSubmit.bind(this)}>
						<h2 class="form-signin-heading">Please Login</h2>
						<EmailFormGroup id="username" label="Username" value={username} required={true}
						                onChange={this.usernameChange.bind(this)} error={usernameError}/>
						<PasswordFormGroup id="password" label="Password" value={password} required={true}
						                   onChange={this.passwordChange.bind(this)} error={passwordError}/>
						<button id="loginUserButton" type="submit" class="btn btn-success">Login</button>
					</form>
				</div>
		);
	}

	usernameChange(e) {
		this.setState({
			username: e.target.value
		});
	}

	passwordChange(e) {
		this.setState({
			password: e.target.value
		});
	}
}
const mapStateToProps = (state) => {
	return {};
};

const mapDispatchToProps = (dispatch) => {
	return {
		login: (username, password, pushToPage) => {
			dispatch(userLogin(username, password, pushToPage));
		}
	};
};

export default connect(mapStateToProps, mapDispatchToProps)(Login);

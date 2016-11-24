import React from "react";
import * as UserActions from "../actions/UserActions";
import {UserEventNames} from "../constants";
import UserStore from "../stores/UserStore";
import {DangerAlert, PageHeader, PasswordFormGroup, TextFormGroup} from "bootstrap-react-components";

export default class Login extends React.Component {

	constructor() {
		super();
		this.state            = {
			username: ""
			, password: ""
			, error: null
			, userLoginBegins: false
		};
		this.userLoginBegins  = this.userLoginBegins.bind(this);
		this.userLoginFailure = this.userLoginFailure.bind(this);
		this.handleChange     = this.handleChange.bind(this);
	}

	componentWillMount() {
		UserStore.on(UserEventNames.USER_LOGIN_FAILURE, this.userLoginFailure);
		UserStore.on(UserEventNames.USER_LOGIN_BEGINS, this.userLoginBegins);
	}

	componentWillUnmount() {
		UserStore.removeListener(UserEventNames.USER_LOGIN_FAILURE, this.userLoginFailure);
		UserStore.removeListener(UserEventNames.USER_LOGIN_BEGINS, this.userLoginBegins);
	}

	userLoginFailure(username, error) {
		this.setState({
			error: error
			, userLoginBegins: false
		})
	}

	userLoginBegins() {
		this.setState({
			userLoginBegins: true
		})
	}

	handleChange(event) {
		switch (event.target.id) {
			case "username":
				this.setState({username: event.target.value});
				break;
			case "password":
				this.setState({password: event.target.value});
				break;
		}
	}

	login() {
		let {username, password} = this.state;
		UserActions.login(username, password);
	}

	render() {
		let {error, usernameError, passwordError, userLoginBegins} = this.state;
		return (
				<div class="login">
					<PageHeader id="login">
						<h1>Login</h1>
					</PageHeader>
					<DangerAlert error={error}/>
					<TextFormGroup error={usernameError}
					               id="username"
					               label="Username"
					               onChange={this.handleChange.bind(this)}
					               placeholder="bob@email.com"/>
					<PasswordFormGroup error={passwordError}
					                   id="password"
					                   label="Password"
					                   onChange={this.handleChange.bind(this)}/>
					<button type="button" class="btn btn-default" onClick={this.login.bind(this)}
					        disabled={userLoginBegins}>Login
					</button>
					<button type="button" class="btn btn-default" disabled={userLoginBegins}>Cancel</button>
				</div>
		);
	}
}
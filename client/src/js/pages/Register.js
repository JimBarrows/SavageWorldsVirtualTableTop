import React from "react";
import {DangerAlert, PageHeader, PasswordFormGroup, TextFormGroup} from "bootstrap-react-components";
import * as UserActions from "../actions/UserActions";
import {UserEventNames} from "../constants";
import UserStore from "../stores/UserStore";

export default class Register extends React.Component {

	constructor() {
		super();
		this.state = {
			username: ""
			, password: ""
			, confirmPassword: ""
			, passwordMatchError: null
			, usernameError: null
			, registerUserSuccess: false
			, error: null
			, registrationBegins: false
		};

		this.handleChange    = this.handleChange.bind(this);
		this.registerBegins  = this.registerBegins.bind(this);
		this.registerFailure = this.registerFailure.bind(this);
	}

	componentWillMount() {
		UserStore.on(UserEventNames.REGISTER_USER_BEGINS, this.registerBegins);
		UserStore.on(UserEventNames.REGISTER_USER_FAILURE, this.registerFailure);
	}

	componentWillUnmount() {
		UserStore.removeListener(UserEventNames.REGISTER_USER_BEGINS, this.registerBegins);
		UserStore.removeListener(UserEventNames.REGISTER_USER_FAILURE, this.registerFailure);
	}

	registerBegins() {
		this.setState({
			registrationBegins: true
			, error: null
		});
	}

	registerFailure(reason) {
		this.setState({
			registerUserSuccess: false
			, registrationBegins: false
			, error: reason
		});
	}

	handleChange(event) {
		switch (event.target.id) {
			case "username":
				this.setState({username: event.target.value});
				break;
			case "password":
				this.setState({password: event.target.value});
				break;
			case "confirmPassword":
				this.setState({confirmPassword: event.target.value});
				break;
		}
	}

	register() {
		let {username, password, confirmPassword} = this.state;
		let valid                                 = true;
		let re                                    = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
		if (re.test(username)) {
			this.setState({
				usernameError: null
			})
		} else {
			this.setState({
				usernameError: "Username is invalid"
			});
			valid = false
		}
		if (password === confirmPassword) {
			this.setState({
				passwordMatchError: null
			})
		} else {
			this.setState({
				passwordMatchError: "Passwords do not match."
			});
			valid = false
		}
		if (valid) {
			UserActions.registerUser(username, password);
		}
	}

	render() {
		let {passwordMatchError, usernameError, error, registrationBegins} = this.state;
		let alertError                                                     = passwordMatchError || (error && error.data);
		return (
				<div class="register">
					<PageHeader >
						<h1>Register</h1>
					</PageHeader>
					<DangerAlert id="registerDangerAlert" error={alertError}/>
					<TextFormGroup label="Username" type="text" placeholder="bob@email.com" id="username"
					               onChange={this.handleChange.bind(this)} error={usernameError}/>
					<PasswordFormGroup label="Password" type="password" id="password" error={passwordError}
					                   onChange={this.handleChange.bind(this)}/>
					<PasswordFormGroup label="Confirm Password" type="password" id="confirmPassword"
					                   onChange={this.handleChange.bind(this)} error={passwordConfirmError}/>

					<button id="registerButton" type="button" class="btn btn-default" onClick={this.register.bind(this)}>
						Register
					</button>
					<button type="button" class="btn btn-default">Cancel</button>
				</div>
		);
	}
}

import {EmailFormGroup, PasswordFormGroup} from 'bootstrap-react-components'
import React from 'react'
import './form_signin.css'

export default class Register extends React.Component {

	state = {
		username            : "",
		password            : "",
		confirmPassword     : "",
		passwordError       : "",
		confirmPasswordError: "",
		isValid             : false
	};

	constructor(props) {
		super(props);
		this.confirmPasswordChange = this.confirmPasswordChange.bind(this);
		this.onSubmit              = this.onSubmit.bind(this);
		this.passwordChange        = this.passwordChange.bind(this);
		this.usernameChange        = this.usernameChange.bind(this);
		this.validateForm          = this.validateForm.bind(this);
	}

	confirmPasswordChange(e) {
		this.setState({
			confirmPassword: e.target.value
		});
	}

	onSubmit(e) {
		e.preventDefault();
		if (this.validateForm()) {
			this.props.history.push('/');
		}
	}

	passwordChange(e) {
		this.setState({
			password: e.target.value
		});
	}

	render() {
		let {username, password, confirmPassword, passwordError, confirmPasswordError} = this.state;
		return (
				<div id="RegisterPage">
					<form id="registrationForm" class="form-signin" onSubmit={this.onSubmit.bind(this)}>
						<h2 class="form-signin-heading">Please Register</h2>
						<EmailFormGroup id="username" label="Username" value={username} required={true}
						                onChange={this.usernameChange}/>
						<PasswordFormGroup id="password" label="Password" value={password} required={true}
						                   onChange={this.passwordChange} error={passwordError}/>
						<PasswordFormGroup id="confirmPassword" label="Confirm Password" value={confirmPassword} required={true}
						                   onChange={this.confirmPasswordChange} error={confirmPasswordError}/>
						<button id="registerUserButton" type="submit" class="btn btn-success">Register</button>
					</form>
				</div>
		);
	}

	usernameChange(e) {
		this.setState({
			username: e.target.value
		});
	}

	validateForm() {
		let {username, password, confirmPassword} = this.state;
		let newState                              = {
			usernameError       : "",
			passwordError       : "",
			confirmPasswordError: "",
			isValid             : false
		};
		const emailRegex                          = /^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/;
		if (!emailRegex.test(username)) {
			newState.usernameError = "Username must be a valid email address";
		}
		if (password !== confirmPassword) {
			newState.passwordError        = "Passwords don't match";
			newState.confirmPasswordError = "Passwords don't match";
		}
		if (!password || (password.length < 8)) {
			newState.passwordError = "Password must be at least 8 characters";
		}
		newState.isValid = !(newState.usernameError && newState.passwordError && newState.confirmPasswordError);
		this.setState(newState);
		return newState.isValid;
	}

}

Register.propTypes = {};

Register.defaultProps = {};
import React from "react";
import {connect} from "react-redux";
import {push} from "react-router-redux";

export function requireAuthentication(Component) {

	class AuthenticatedComponent extends React.Component {

		componentWillMount() {
			this.checkAuth();
		}

		componentWillReceiveProps(nextProps) {
			this.checkAuth();
		}

		checkAuth() {
			if (!this.props.isAuthenticated) {
				let redirectAfterLogin = this.props.location.pathname;
				this.props
						.dispatch(push(`/login?next=${redirectAfterLogin}`));
			}
		}

		render() {
			return (
					<div>
						{this.props.isAuthenticated === true
								? <Component {...this.props}/>
								: null
						}
					</div>
			)

		}
	}

	const mapStateToProps = (state) => ({
		token: state.user.token,
		userName: state.user.userName,
		isAuthenticated: state.user.isAuthenticated
	});

	return connect(mapStateToProps)(AuthenticatedComponent);

}

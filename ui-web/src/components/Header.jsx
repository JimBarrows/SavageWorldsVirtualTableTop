import React from 'react';
import {Link} from 'react-router-dom';

// import {withRouter} from 'react-router-dom'

class Header extends React.Component {

	constructor() {
		super();
		this.state = {
			collapsed: true,
			username : null
		};
	}

	toggleCollapse() {
		let collapsed = !this.state.collapsed;
		this.setState({collapsed});
	}

	logout(e) {
		e.preventDefault();
		this.props.logout();
	}

	render() {
		// const {auth}      = this.props;
		// let UserComponent = auth.isAuthenticated
		// 		? (
		// 				<ul className='nav navbar-nav navbar-right'>
		// 					<li>
		// 						<a id='logoutButton' href='/' onClick={this.logout.bind(this)}>
		// 							<span className='glyphicon glyphicon-off' aria-hidden='true'></span>
		// 						</a>
		// 					</li>
		// 				</ul>
		// 		)
		// 		:


		return (
				<nav className='navbar navbar-inverse navbar-fixed-top'>
					<div className='container'>
						<div className='navbar-header'>
							<button type='button' className='navbar-toggle collapsed' data-toggle='collapse' data-target='#navbar'
							        aria-expanded='false' aria-controls='navbar' onClick={this.toggleCollapse.bind(this)}>
								<span className='sr-only'>Toggle navigation</span>
								<span className='icon-bar'></span>
								<span className='icon-bar'></span>
								<span className='icon-bar'></span>
							</button>
							<a className='navbar-brand' href='/'>Savage Worlds</a>
						</div>
						<div id='navbar' className={'navbar-collapse collapse'}>
							<ul className='nav navbar-nav'>
							</ul>
							<ul className='nav navbar-nav navbar-right'>
								<li><Link to='/register'>Register</Link></li>
							</ul>
							;
						</div>
					</div>
				</nav>
		);
	}
}

Header.propTypes = {};

Header.defaultProps = {};

// const mapStateToProps = (state, ownProps) => {
// 	return {
// 		app     : state.app,
// 		location: ownProps.location,
// 		auth    : state.auth
// 	};
// };
//
// const mapDispatchToProps = (dispatch) => {
// 	return {
// 		logout: () => {
// 			// dispatch(logoutAndRedirect())
// 		}
// 	};
// };
export default Header; //withRouter(connect(mapStateToProps, mapDispatchToProps)(Header))

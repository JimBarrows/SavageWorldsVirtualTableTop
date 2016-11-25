import React from "react";
import {Link} from "react-router";

class Header extends React.Component {

	constructor() {
		super();
		this.state = {
			collapsed: true
			, username: null
		};
	}

	componentWillMount() {
	}

	componentWillUnmount() {
	}

	toggleCollapse() {
		let collapsed = !this.state.collapsed;
		this.setState({collapsed});
	}

	logout() {
	}

	render() {
		let {collapsed, username} = this.state;
		const navClass            = collapsed ? "collapse" : "";
		const {location}          = this.props;
		const plotPointClass      = location.pathname === "/" ? "active" : "";
		const registerClass       = location.pathname.match(/^\/register/) ? "active" : "";
		const loginClass          = location.pathname.match(/^\/login/) ? "active" : "";
		let UserComponent         = null;
		if (username) {
			UserComponent = (
					<ul class="nav navbar-nav navbar-right">
						<li><p class="navbar-text">{username}</p></li>
						<li><Link to="settings"><span class="glyphicon glyphicon-cog" aria-hidden="true"></span></Link></li>
						<li>
							<a onClick={this.logout.bind(this)}>
									<span
											class="glyphicon glyphicon-off" aria-hidden="true"></span></a>
						</li>
					</ul>);
		} else {
			UserComponent = (
					<ul class="nav navbar-nav navbar-right">
						<li class={registerClass}><Link to="register">Register</Link></li>
						<li class={loginClass}><Link to="login">Login</Link></li>
					</ul>
			)
		}
		return (
				<nav class="navbar navbar-inverse navbar-fixed-top">
					<div class="container">
						<div class="navbar-header">
							<button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar"
							        aria-expanded="false" aria-controls="navbar" onClick={this.toggleCollapse.bind(this)}>
								<span class="sr-only">Toggle navigation</span>
								<span class="icon-bar"></span>
								<span class="icon-bar"></span>
								<span class="icon-bar"></span>
							</button>
							<a class="navbar-brand" href="#">{this.props.title}</a>
						</div>
						<div id="navbar" class={"navbar-collapse " + navClass}>
							<ul class="nav navbar-nav">
								<li class={plotPointClass}><Link to="/">PlotPoints</Link></li>
							</ul>
							{UserComponent}
						</div>
					</div>
				</nav>
		);
	}
}
export default Header;

import React from 'react'

class Header extends React.Component {

	static propTypes = {};

	static defaultProps = {};

	state = {
		collapsed: true,
		username : null
	};

	toggleCollapse = () => {
		let collapsed = !this.state.collapsed;
		this.setState({collapsed});
	}

	logout = (e) => {
		e.preventDefault();
		this.props.logout();
	}

	render() {
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
							</ul>
						</div>
					</div>
				</nav>
		);
	}
}

export default Header;

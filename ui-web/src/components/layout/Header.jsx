import React from 'react'

class Header extends React.Component {

  static propTypes = {}

  static defaultProps = {}

  state = {
    collapsed: true,
    username : null
  }

  toggleCollapse = () => {
    let collapsed = !this.state.collapsed
    this.setState({collapsed})
  }

  logout = (e) => {
    e.preventDefault()
    this.props.logout()
  }

  render() {
    let id = 'SavageWorldsVirtualTableTop'
	  return <nav className="navbar navbar-dark sticky-top bg-dark flex-md-nowrap p-0">
		  <a className="navbar-brand col-sm-3 col-md-2 mr-0" href="#">Savage Worlds Virtual Table Top</a>
		  <ul className="navbar-nav px-3">
			  <li className="nav-item text-nowrap">
				  <a className="nav-link" href="#">Sign out</a>
			  </li>
		  </ul>
	  </nav>
  }
}

export default Header

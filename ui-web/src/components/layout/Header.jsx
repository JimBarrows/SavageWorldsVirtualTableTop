import {Navbar, NavbarBrand} from 'bootstrap-react-components'
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
    return <Navbar id={id}>
      <NavbarBrand id={id} onClick={() => true}>Savage Worlds Virtual Table Top</NavbarBrand>
    </Navbar>
  }
}

export default Header

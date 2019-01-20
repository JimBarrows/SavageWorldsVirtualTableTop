import Navbar    from 'bootstrap-react-components/distribution/bootstrap/components/Navbar'
import Brand     from 'bootstrap-react-components/distribution/bootstrap/components/Navbar/Brand'
import PropTypes from 'prop-types'
import React     from 'react'

export default class Header extends React.Component {

	static propTypes = {
		id              : PropTypes.string.isRequired,
		indexLinkClicked: PropTypes.func.isRequired,
	}

	static defaultProps = {}

	brandClicked = () => this.props.indexLinkClicked

	render () {
		return (
			<Navbar id={'header-' + this.props.id} >
				<Brand id={'header-' + this.props.id} onClick={this.brandClicked} >Savage Worlds</Brand >
			</Navbar >)
	}
}


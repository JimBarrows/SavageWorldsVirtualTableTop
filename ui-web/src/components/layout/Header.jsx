import Navbar from 'bootstrap-react-components/distribution/bootstrap/components/Navbar'
import Brand  from 'bootstrap-react-components/distribution/bootstrap/components/Navbar/Brand'
import React  from 'react'

export default class Header extends React.Component {

	static propTypes = {}

	static defaultProps = {}

	render () {
		return (
			<Navbar id={'main'} >
				<Brand id={'main'} >Savage Worlds</Brand >
			</Navbar >)
	}
}


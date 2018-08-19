import PropTypes from 'prop-types'
import React from 'react'

export default class Navigation extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id: PropTypes.string.isRequired
	}

	render() {
		let {id}         = this.props
		let component_id = `Navigation-${id}`
		return (
			<nav id={component_id} className="col-md-2 d-none d-md-block bg-light sidebar">
				<div className="sidebar-sticky">
					<ul className="nav flex-column">
						<li className="nav-item">
							<a className="nav-link active" href="#">
								Plot Point <span className="sr-only">(current)</span>
							</a>
						</li>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Setting Rules
							</a>
						</li>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Races
							</a>
						</li>
						<h6 className="sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted">
							<span>Gear</span>
							<a className="d-flex align-items-center text-muted" href="#">
							</a>
						</h6>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Mundane Items
							</a>
						</li>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Hand Weapons
							</a>
						</li>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Armor
							</a>
						</li>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Ranged Weapons
							</a>
						</li>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Vehicle Mounted & AT Guns
							</a>
						</li>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Ammunition
							</a>
						</li>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Special Weapons
							</a>
						</li>
						<h6 className="sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted">
							<span>Vehicles</span>
							<a className="d-flex align-items-center text-muted" href="#">
							</a>
						</h6>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Ground
							</a>
						</li>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Water
							</a>
						</li>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Air
							</a>
						</li>
						<h6 className="sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted">
							<span>Powers</span>
							<a className="d-flex align-items-center text-muted" href="#">
							</a>
						</h6>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Arcane Backgrounds
							</a>
						</li>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Trappings
							</a>
						</li>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Powers
							</a>
						</li>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Beasts
							</a>
						</li>
						<li className="nav-item">
							<a className="nav-link" href="#">
								Characters
							</a>
						</li>
					</ul>
				</div>
			</nav>
		)
	}
}


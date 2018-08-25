import PropTypes from 'prop-types'
import React from 'react'
import ALink from '../ALink'

export default class Navigation extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id: PropTypes.string.isRequired,
		navigateTo: PropTypes.func.isRequired
	}

	navigateTo = name => this.props.navigateTo(name)

	render() {
		let {id}         = this.props
		let component_id = `Navigation-${id}`
		return (
			<nav id={component_id} className="col-md-2 d-none d-md-block bg-light sidebar">
				<div className="sidebar-sticky">
					<ul className="nav flex-column">
						<li className="nav-item">
							<ALink id={component_id + 'PlotPoint'} className="nav-link active"
							       onClick={e => {this.navigateTo('PlotPoint')}}>
								Plot Point <span className="sr-only">(current)</span>
							</ALink>
						</li>
						<li className="nav-item">
							<ALink id={component_id + 'SettingRules'} className="nav-link"
							       onClick={e => this.navigateTo('SettingRules')}>
								Setting Rules
							</ALink>
						</li>
						<h6 className="sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted">
							<span>Character Creation</span>
						</h6>
						<li className="nav-item">
							<ALink id={component_id + 'Races'} className="nav-link" onClick={e => this.navigateTo('Races')}>
								Races
							</ALink>
						</li>
						<li className="nav-item">
							<ALink id={component_id + 'Skills'} className="nav-link" onClick={e => this.navigateTo('Skills')}>
								Skills
							</ALink>
						</li>
						<li className="nav-item">
							<ALink id={component_id + 'Hindrances'} className="nav-link" onClick={e => this.navigateTo('Hindrances')}>
								Hindrances
							</ALink>
						</li>
						<li className="nav-item">
							<ALink id={component_id + 'Edgess'} className="nav-link" onClick={e => this.navigateTo('Edges')}>
								Edges
							</ALink>
						</li>
						<h6 className="sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted">
							<span>Gear</span>
						</h6>
						<li className="nav-item">
							<ALink id={component_id + 'MundaneItems'} className="nav-link"
							       onClick={e => this.navigateTo('MundaneItems')}>
								Mundane Items
							</ALink>
						</li>
						<li className="nav-item">
							<ALink id={component_id + 'HandWeapons'} className="nav-link"
							       onClick={e => this.navigateTo('HandWeapons')}>
								Hand Weapons
							</ALink>
						</li>
						<li className="nav-item">
							<ALink id={component_id + 'Armor'} className="nav-link" onClick={e => this.navigateTo('Armor')}>
								Armor
							</ALink>
						</li>
						<li className="nav-item">
							<ALink id={component_id + 'Armor'} className="nav-link" onClick={e => this.navigateTo('RangedWeapons')}>
								Ranged Weapons
							</ALink>
						</li>
						<li className="nav-item">
							<ALink id={component_id + 'VehicleMountedAndAtGuns'} className="nav-link"
							       onClick={e => this.navigateTo('VehicleMountedAndAtGuns')}>
								Vehicle Mounted & AT Guns
							</ALink>
						</li>
						<li className="nav-item">
							<ALink id={component_id + 'Ammunition'} className="nav-link" onClick={e => this.navigateTo('Ammunition')}>
								Ammunition
							</ALink>
						</li>
						<li className="nav-item">
							<ALink id={component_id + 'SpecialWeapons'} className="nav-link"
							       onClick={e => this.navigateTo('SpecialWeapons')}>
								Special Weapons
							</ALink>
						</li>
						<h6 className="sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted">
							<span>Vehicles</span>
						</h6>
						<li className="nav-item">
							<ALink id={component_id + 'GroundVehicle'} className="nav-link"
							       onClick={e => this.navigateTo('GroundVehicle')}>
								Ground
							</ALink>
						</li>
						<li className="nav-item">
							<ALink id={component_id + 'WaterVehicle'} className="nav-link"
							       onClick={e => this.navigateTo('WaterVehicle')}>
								Water
							</ALink>
						</li>
						<li className="nav-item">
							<ALink id={component_id + 'AirVehicle'} className="nav-link" onClick={e => this.navigateTo('AirVehicle')}>
								Air
							</ALink>
						</li>
						<h6 className="sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted">
							<span>Powers</span>
						</h6>
						<li className="nav-item">
							<ALink id={component_id + 'ArcaneBackground'} className="nav-link"
							       onClick={e => this.navigateTo('ArcaneBackground')}>
								Arcane Backgrounds
							</ALink>
						</li>
						<li className="nav-item">
							<ALink id={component_id + 'Trapping'} className="nav-link" onClick={e => this.navigateTo('Trapping')}>
								Trappings
							</ALink>
						</li>
						<li className="nav-item">
							<ALink id={component_id + 'Powers'} className="nav-link" onClick={e => this.navigateTo('Powers')}>
								Powers
							</ALink>
						</li>
						<h6>Allies, Critters & Enemies</h6>
						<li className="nav-item">
							<ALink id={component_id + 'Beasts'} className="nav-link" onClick={e => this.navigateTo('Beasts')}>
								Beasts
							</ALink>
						</li>
						<li className="nav-item">
							<ALink id={component_id + 'Characters'} className="nav-link" onClick={e => this.navigateTo('Characters')}>
								Characters
							</ALink>
						</li>
					</ul>
				</div>
			</nav>
		)
	}
}

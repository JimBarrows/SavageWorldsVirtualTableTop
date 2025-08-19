import Nav       from 'bootstrap-react-components/distribution/bootstrap/components/Nav'
import NavItem   from 'bootstrap-react-components/distribution/bootstrap/components/Nav/NavItem'
import PropTypes from 'prop-types'
import React     from 'react'

export default class Navigation extends React.Component {

	static defaultProps = {}

	static propTypes = {
		id        : PropTypes.string.isRequired,
		navigateTo: PropTypes.func.isRequired
	}

	state = {
		active: 'BasicRules'
	}

	navigateTo = (name) => () => {
		this.setState({active: name})
		return this.props.navigateTo(name)
	}

	render () {
		let {id}         = this.props
		let component_id = `Navigation-${id}`
		return (<Nav id={component_id} position={'verticle'} look={'tabs'} >
			<NavItem id={component_id + '-basic-rules'} onClick={this.navigateTo('BasicRules')} label={'Basic Rules'}
				state={this.state.active === 'BasicRules' ? 'active' : 'enabled'} />
			<NavItem id={component_id + '-setting-rules'} onClick={this.navigateTo('SettingRules')} label={'Setting Rules'}
				state={this.state.active === 'SettingRules' ? 'active' : 'enabled'} />
			<NavItem id={component_id + '-skills'} onClick={this.navigateTo('Skills')} label={'Skills'}
				state={this.state.active === 'Skills' ? 'active' : 'enabled'} />
			<NavItem id={component_id + '-edges'} onClick={this.navigateTo('Edges')} label={'Edges'}
				state={this.state.active === 'Edges' ? 'active' : 'enabled'} />
			<NavItem id={component_id + '-hindrances'} onClick={this.navigateTo('Hindrances')} label={'Hindrances'}
				state={this.state.active === 'Hindrances' ? 'active' : 'enabled'} />
			<NavItem id={component_id + '-gear'} onClick={this.navigateTo('Gear')} label={'Gear'}
				state={this.state.active === 'Gear' ? 'active' : 'enabled'} />
			<NavItem id={component_id + '-powers'} onClick={this.navigateTo('Powers')} label={'Powers'}
				state={this.state.active === 'Powers' ? 'active' : 'enabled'} />
			<NavItem id={component_id + '-races'} onClick={this.navigateTo('Races')} label={'Races'}
				state={this.state.active === 'Races' ? 'active' : 'enabled'} />
			<NavItem id={component_id + '-beasts'} onClick={this.navigateTo('Beasts')} label={'Beasts'}
				state={this.state.active === 'Beasts' ? 'active' : 'enabled'} />
			<NavItem id={component_id + '-characters'} onClick={this.navigateTo('Characters')} label={'Characters'}
				state={this.state.active === 'Characters' ? 'active' : 'enabled'} />
			<NavItem id={component_id + '-dramatic-personae'} onClick={this.navigateTo('DramaticPersonae')} label={'Dramatic Personae'}
				state={this.state.active === 'DramaticPersonae' ? 'active' : 'enabled'} />
		</Nav >)

		// 				<span>Character Creation</span>
		// 			</h6>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'Races'} className="nav-link" onClick={e => this.navigateTo('Races')}>
		// 					Races
		// 				</ALink>
		// 			</li>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'Skills'} className="nav-link" onClick={e => this.navigateTo('Skills')}>
		// 					Skills
		// 				</ALink>
		// 			</li>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'Hindrances'} className="nav-link" onClick={e => this.navigateTo('Hindrances')}>
		// 					Hindrances
		// 				</ALink>
		// 			</li>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'Edges'} className="nav-link" onClick={e => this.navigateTo('Edges')}>
		// 					Edges
		// 				</ALink>
		// 			</li>
		// 			<h6 className="sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted">
		// 				<span>Gear</span>
		// 			</h6>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'MundaneItems'} className="nav-link"
		// 				       onClick={e => this.navigateTo('MundaneItems')}>
		// 					Mundane Items
		// 				</ALink>
		// 			</li>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'HandWeapons'} className="nav-link"
		// 				       onClick={e => this.navigateTo('HandWeapons')}>
		// 					Hand Weapons
		// 				</ALink>
		// 			</li>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'Armor'} className="nav-link" onClick={e => this.navigateTo('Armor')}>
		// 					Armor
		// 				</ALink>
		// 			</li>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'Armor'} className="nav-link" onClick={e => this.navigateTo('RangedWeapons')}>
		// 					Ranged Weapons
		// 				</ALink>
		// 			</li>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'VehicleMountedAndAtGuns'} className="nav-link"
		// 				       onClick={e => this.navigateTo('VehicleMountedAndAtGuns')}>
		// 					Vehicle Mounted & AT Guns
		// 				</ALink>
		// 			</li>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'Ammunition'} className="nav-link" onClick={e => this.navigateTo('Ammunition')}>
		// 					Ammunition
		// 				</ALink>
		// 			</li>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'Index'} className="nav-link"
		// 				       onClick={e => this.navigateTo('Index')}>
		// 					Special Weapons
		// 				</ALink>
		// 			</li>
		// 			<h6 className="sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted">
		// 				<span>Vehicles</span>
		// 			</h6>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'GroundVehicle'} className="nav-link"
		// 				       onClick={e => this.navigateTo('GroundVehicles')}>
		// 					Ground
		// 				</ALink>
		// 			</li>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'WaterVehicle'} className="nav-link"
		// 				       onClick={e => this.navigateTo('WaterVehicles')}>
		// 					Water
		// 				</ALink>
		// 			</li>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'Aircraft'} className="nav-link" onClick={e => this.navigateTo('Aircraft')}>
		// 					Air
		// 				</ALink>
		// 			</li>
		// 			<h6 className="sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted">
		// 				<span>Powers</span>
		// 			</h6>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'ArcaneBackground'} className="nav-link"
		// 				       onClick={e => this.navigateTo('ArcaneBackground')}>
		// 					Arcane Backgrounds
		// 				</ALink>
		// 			</li>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'Trapping'} className="nav-link"
		// 				       onClick={e => this.navigateTo('TrappingsAndEffects')}>
		// 					Trappings & Effects
		// 				</ALink>
		// 			</li>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'Powers'} className="nav-link" onClick={e => this.navigateTo('Powers')}>
		// 					Powers
		// 				</ALink>
		// 			</li>
		// 			<h6 className="sidebar-heading d-flex justify-content-between align-items-center px-3 mt-4 mb-1 text-muted">
		// 				<span>Allies, Critters & Enemies</span></h6>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'Beasts'} className="nav-link" onClick={e => this.navigateTo('Beasts')}>
		// 					Beasts
		// 				</ALink>
		// 			</li>
		// 			<li className="nav-item">
		// 				<ALink id={component_id + 'Characters'} className="nav-link" onClick={e => this.navigateTo('Characters')}>
		// 					Characters
		// 				</ALink>
		// 			</li>
		// 		</ul>
		// 	</div>
		// </nav>

	}
}

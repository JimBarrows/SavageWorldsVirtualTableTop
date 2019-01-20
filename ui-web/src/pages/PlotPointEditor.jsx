import {PageHeader} from 'bootstrap-react-components'
import React        from 'react'
import PlotPoint    from '../components/PlotPointEditor/PlotPoint'


export default class PlotPointEditor extends React.Component {

	static defaultProps = {
		id: 'PlotPointEditorPage'
	}

	constructor (props) {
		super(props)
		this.save = this.save.bind(this)
	}

	state = {
		aircraft               : [],
		ammunition             : [],
		arcaneBackgrounds      : [],
		armor                  : [],
		basicRules             : {
			maximumAttributePoints: 5,
			maximumMajorHindrances: 1,
			maximumMinorHindrances: 2,
			maximumSkillPoints    : 15
		},
		beasts                 : [],
		description            : '',
		edges                  : [],
		groundVehicles         : [],
		handWeapons            : [],
		hindrances             : [],
		maximumAttributePoints : 5,
		maximumMajorHindrances : 1,
		maximumMinorHindrances : 2,
		maximumSkillPoints     : 15,
		mundaneItems           : [],
		name                   : '',
		powers                 : [],
		races                  : [],
		rangedWeapons          : [],
		settingRules           : [],
		skills                 : [],
		specialWeapons         : [],
		trappingsAndEffects    : [],
		vehicleMountedAndAtGuns: [],
		watercraft             : []
	}

	save = async e => {
		let toSave = {
			aircraft               : this.state.aircraft,
			ammunition             : this.state.ammunition,
			arcaneBackgrounds      : this.state.arcaneBackgrounds,
			armor                  : this.state.armor,
			beasts                 : this.state.beasts,
			description            : this.state.description,
			edges                  : this.state.edges,
			groundVehicles         : this.state.groundVehicles,
			handWeapons            : this.state.handWeapons,
			hindrances             : this.state.hindrances,
			maximumAttributePoints : this.state.maximumAttributePoints,
			maximumMajorHindrances : this.state.maximumMajorHindrances,
			maximumMinorHindrances : this.state.maximumMinorHindrances,
			maximumSkillPoints     : this.state.maximumSkillPoints,
			mundaneItems           : this.state.mundaneItems,
			name                   : this.state.name,
			powers                 : this.state.powers,
			races                  : this.state.races,
			rangedWeapons          : this.state.rangedWeapons,
			skills                 : this.state.skills,
			specialWeapons         : this.state.specialWeapons,
			trappingsAndEffects    : this.state.trappingsAndEffects,
			vehicleMountedAndAtGuns: this.state.vehicleMountedAndAtGuns,
			watercraft             : this.state.watercraft
		}
		if (this.props.match.params.name) {
			// await API.put('PlotPointsCRUD', `/PlotPoints`, {
			// 	body: {...toSave}
			// })
		} else {
			// await API.post('PlotPointsCRUD', `/PlotPoints`, {
			// 	body: {...toSave}
			// })
		}
	}

	render () {
		return <div id={this.props.id} >
			<PageHeader id={this.props.id} ><h1 >Plot Point Editor</h1 ></PageHeader >
			<form id='plotPointForm' >
				<PlotPoint id={'plotPointForm'} onChange={this.save} plotPoint={this.state} />
			</form >
		</div >
	}

	async componentDidMount () {
		if (this.props.match.params.name) {
			// let plotPoint = await API.get('PlotPointsCRUD', `/PlotPoints/object/${this.props.match.params.name}`)
			let plotPoint = {}
			this.setState({
											...plotPoint
										})
		}
	};
}



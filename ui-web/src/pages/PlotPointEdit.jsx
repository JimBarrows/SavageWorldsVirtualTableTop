import {API}          from 'aws-amplify'
import {PageHeader}   from 'bootstrap-react-components'
import * as PropTypes from 'prop-types'
import React          from 'react'
import PlotPointForm  from '../components/plotpoint/editor'


export default class PlotPointEdit extends React.Component {

	static propTypes    = {
		id: PropTypes.string
	}
	static defaultProps = {
		id: 'PlotPointEditorPage'
	}

	cancel = async () => {

	}

	save = async plotPoint => {

		await API.put('PlotPointsCRUD', `/PlotPoints`, {
			plotPoint
		})
		this.props.history.push('/')
	}

	async componentDidMount () {
		let plotPoint = await API.get('PlotPointsCRUD', `/PlotPoints/object/${this.props.match.params.name}`, {})
		this.setState({
										plotPoint
									})
	}

	render () {
		return <div id={this.props.id} >
			<PageHeader id={this.props.id} ><h1 >Edit Plot Point - {this.state.name}</h1 ></PageHeader >
			<PlotPointForm id={'plotPointForm'} onSave={this.save} onCancel={this.cancel} onChange={this.save}
				plotPoint={this.state} />
		</div >
	}

	state = {
		basicRules  : {
			maximumAttributePoints: 5,
			maximumMajorHindrances: 1,
			maximumMinorHindrances: 2,
			maximumSkillPoints    : 15
		},
		beasts      : [],
		description : '',
		edges       : [],
		gear        : {
			aircraft               : [],
			ammunition             : [],
			armor                  : [],
			groundVehicles         : [],
			handWeapons            : [],
			mundaneItems           : [],
			rangedWeapons          : [],
			skills                 : [],
			specialWeapons         : [],
			trappingsAndEffects    : [],
			vehicleMountedAndAtGuns: [],
			watercraft             : []
		},
		hindrances  : [],
		name        : '',
		powers      : {
			arcaneBackgrounds: [],
		},
		races       : [],
		settingRules: []
	}


}



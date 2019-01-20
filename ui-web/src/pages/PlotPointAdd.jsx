import {PageHeader}   from 'bootstrap-react-components'
import * as PropTypes from 'prop-types'
import React          from 'react'
import PlotPointForm  from '../components/plotpoints/Editor'


export default class PlotPointEdit extends React.Component {

	static propTypes    = {
		id: PropTypes.string
	}
	static defaultProps = {
		id: 'PlotPointEditorPage'
	}

	cancel = async () => {

	}

	async componentDidMount () {

	}

	save = async () => {
	}

	render () {
		return <div id={this.props.id} >
			<PageHeader id={this.props.id} ><h1 >New Plot Point</h1 ></PageHeader >
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



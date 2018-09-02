import {API} from 'aws-amplify'
import {PageHeader} from 'bootstrap-react-components'
import React from 'react'
import {withRouter} from 'react-router'
import Form from '../components/PlotPointEditor/index'
import Navigation from '../components/PlotPointEditor/Navigation'
import './PlotPointEditor.css'


class PlotPointEditor extends React.Component {

	static defaultProps = {
		id: 'PlotPointEditorPage'
	}

	state    = {
		plotPoint: {
			aircraft               : [],
			ammunition             : [],
			arcaneBackgrounds      : [],
			armor                  : [],
			beasts                 : [],
			characters             : [],
			display_section        : 'main',
			description            : ' ',
			edges                  : [],
			groundVehicles         : [],
			handWeapons            : [],
			hindrances             : [],
			maximumAttributePoints : 5,
			maximumMajorHindrances : 1,
			maximumMinorHindrances : 2,
			maximumSkillPoints     : 15,
			mundaneItems           : [],
			name                   : ' ',
			powers                 : [],
			races                  : [],
			rangedWeapons          : [],
			settingRules           : [],
			skills                 : [],
			specialWeapons         : [],
			trappingsAndEffects    : [],
			vehicleMountedAndAtGuns: [],
			watercraft             : []
		},
		section:  'PlotPoint'
	}
	onChange = plotPoint => this.setState({plotPoint})
	save     = async body => {

		if (this.props.match.params.name) {
			await API.put('PlotPointsCRUD', `/PlotPoints`, {
				body
			})
		} else {
			await API.post('PlotPointsCRUD', `/PlotPoints`, {
				body
			})
		}

		this.props.history.push('/')
	}

	sectionChange = section => this.setState(({
		section
	}))

	cancel   = () => this.props.history.push('/')

	async componentDidMount() {
		if (this.props.match.params.name) {
			let plotPoint = await API.get('PlotPointsCRUD', `/PlotPoints/object/${this.props.match.params.name}`, {})
			this.setState({
				plotPoint
			})
		}
	};

	render() {
		let componentId = `PlotPointEditor-${this.props.id}`
		return <div id={componentId}>
			<div className="row">
				<div className="col-md-12">
					<PageHeader id={componentId}><h1>Plot Point Editor</h1></PageHeader>
				</div>
			</div>
			<div className="row">
				<Navigation id={componentId} navigateTo={this.sectionChange}/>
				<Form id={componentId} cancel={this.cancel} plotPoint={this.state.plotPoint} onChange={this.onChange}
				      save={this.save} show={this.state.section}/>
			</div>
		</div>
	}
}

export default withRouter(PlotPointEditor)

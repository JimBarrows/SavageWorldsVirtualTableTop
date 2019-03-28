import React, {Component} from 'react'
import {Route, Switch}    from 'react-router-dom'
import './App.css'
import Header             from './components/layout/Header'
import PlotPointAdd       from './pages/PlotPointAdd'
import PlotPointEdit      from './pages/PlotPointEdit'
import PlotPointList      from './pages/PlotPointList'


export default class App extends Component {

	gotoIndex = () => this.props.history.push('/')

	render () {
		return (
			<div >
				<Header id={'app'} indexLinkClicked={this.gotoIndex} />
				<div id={"layout"} className="container" role={"main"} >
					<Switch >
						<Route exact path="/" component={PlotPointList} />
						<Route exact path='/plot_point/add' component={PlotPointAdd} />
						<Route exact path={"/plot_point/:name/edit"} component={PlotPointEdit} />
					</Switch >
				</div >
			</div >
		)
	}
}


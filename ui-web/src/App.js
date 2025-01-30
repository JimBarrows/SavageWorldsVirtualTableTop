import Amplify, {Storage}                       from 'aws-amplify'
import {withAuthenticator}                      from 'aws-amplify-react'
import React, {Component}                       from 'react'
import {BrowserRouter as Router, Route, Routes} from 'react-router-dom'
import './App.css'
// import awsmobile                                from './aws-exports'
import Header                                   from './components/layout/Header'
import PlotPointAdd                             from './pages/PlotPointAdd'
import PlotPointEdit                            from './pages/PlotPointEdit'
import PlotPointList                            from './pages/PlotPointList'

// Amplify.configure(awsmobile)
Storage.configure({level: 'private'})

var gotoIndex = () => this.props.history.push('/')

export default function App () {
// class App extends Component {


	// render () {
	return (
		<Router >
			<div >
				<Header id = {'app'} indexLinkClicked = {gotoIndex()} />
				<div id = {"layout"} className = "container" role = {"main"} >
					<Routes >
						<Route exact path = "/" element = {<PlotPointList />} />
						<Route exact path = "/plot_point/add" element = {<PlotPointAdd />} />
						<Route exact path = {"/plot_point/:name/edit"} element = {<PlotPointEdit />} />
					</Routes >
				</div >
			</div >
		</Router >
	)
	// }
}

// export default withAuthenticator(App, true)


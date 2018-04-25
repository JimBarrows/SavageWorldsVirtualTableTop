import axios from 'axios';
import React, {Component} from 'react';
import {Route, Switch} from 'react-router-dom';
import './App.css';
import Header from './components/Header';
import MessageDisplay from './components/MessageDisplay';
import Register from './components/Register';
import PlotPointEditor from './pages/PlotPointEditor';
import PlotPointList from './pages/PlotPointList';

axios.create({
	baseURL       : '/api/',
	validateStatus: function (status) {
		return status < 300;
	}
});

class App extends Component {
	render() {
		return (
				<div>
					<Header/>
					<MessageDisplay id={'application'}/>
					<div id={"layout"} className="container" role={"main"}>
						<Switch>
							<Route exact path="/" component={PlotPointList}/>
							<Route exact path='/plotPointEditor' component={PlotPointEditor}/>
							<Route exact path={"/plotPointEditor/:name"} component={PlotPointEditor}/>
							<Route exact path="/register" component={Register}/>
						</Switch>
					</div>
				</div>
		);
	}
}

export default App;

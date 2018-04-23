import axios from 'axios';
import React, {Component} from 'react';
import {Route, Switch} from 'react-router-dom';
import './App.css';
import Header from './components/Header';
import MessageDisplay from './components/MessageDisplay';
import Register from './components/Register';
import PlotPointEditor from './pages/PlotPointEditor';
import PlotPoints from './pages/PlotPoints';

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
							<Route exact path="/" component={PlotPoints}/>
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

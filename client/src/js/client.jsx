import App from "./containers/layout";
import axios from "axios";
import {browserHistory, IndexRoute, Route, Router} from "react-router";
import Login from "./containers/Login";
import PlotPoints from "./containers/PlotPoints";
import {Provider} from "react-redux";
import React from "react";
import ReactDOM from "react-dom";
import Register from "./containers/Register";
import store from "./Store";
import {syncHistoryWithStore} from "react-router-redux";
import "bootstrap/dist/css/bootstrap.min.css";
import "font-awesome/css/font-awesome.min.css";

axios.create({
	validateStatus: function (status) {
		return status < 300;
	}
});

const history = syncHistoryWithStore(browserHistory, store);

const app = document.getElementById('app');

ReactDOM.render(
		<Provider store={store}>
			<Router history={history}>
				<Route path="/" component={App}>
					<IndexRoute component={PlotPoints}></IndexRoute>
					<Route path="register" component={Register}/>
					<Route path="login" component={Login}/>
				</Route>
			</Router>
		</Provider>
		, app);
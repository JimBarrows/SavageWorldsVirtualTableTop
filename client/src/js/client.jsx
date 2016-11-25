import App from "./containers/layout";
import axios from "axios";
import {browserHistory, IndexRoute, Route, Router} from "react-router";
import PlotPoints from "./containers/PlotPoints";
import {Provider} from "react-redux";
import React from "react";
import ReactDOM from "react-dom";
import store from "./Store";
import "bootstrap/dist/css/bootstrap.min.css";
import "font-awesome/css/font-awesome.min.css";

axios.create({
	validateStatus: function (status) {
		return status < 300;
	}
});

const app = document.getElementById('app');

ReactDOM.render(
		<Provider store={store}>
			<Router history={browserHistory}>
				<Route path="/" component={App}>
					<IndexRoute component={PlotPoints}></IndexRoute>
				</Route>
			</Router>
		</Provider>
		, app);
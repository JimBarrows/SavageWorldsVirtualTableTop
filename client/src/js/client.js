import React from "react";
import ReactDOM from "react-dom";
import {Router, Route, IndexRoute, hashHistory} from "react-router";
import Layout from "./components/Layout";
import Settings from "./pages/Settings";
import PlotPointAdd from "./pages/PlotPointAdd";
import PlotPointEdit from "./pages/PlotPointEdit";
import PlotPoints from "./pages/PlotPoints";
import Register from "./pages/Register";
import Login from "./pages/Login";
import UserStore from "./stores/UserStore";
import "bootstrap/dist/css/bootstrap.min.css";

const app = document.getElementById('app');

function requireAuth(nextState, replace) {
	if (!UserStore.user()) {
		replace({
			pathname: '/login',
			state: {nextPathname: nextState.location.pathname}
		})
	}
}

ReactDOM.render(<Router history={hashHistory}>
	<Route path="/" component={Layout}>
		<IndexRoute component={PlotPoints} onEnter={requireAuth}></IndexRoute>
		<Route path="plotPoint/add" name="plotPointAdd" component={PlotPointAdd} onEnter={requireAuth}></Route>
		<Route path="plotPoint/edit" name="plotPointEdit" component={PlotPointEdit}
		       onEnter={requireAuth}></Route>
		<Route path="settings" name="settings" component={Settings} onEnter={requireAuth}></Route>
		<Route path="register" name="register" component={Register}></Route>
		<Route path="login" name="login" component={Login}></Route>
	</Route>
</Router>, app);
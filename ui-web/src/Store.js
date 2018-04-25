import {createStore, applyMiddleware} from "redux";
import reducers from "./reducers/index";
import {createLogger} from "redux-logger";
import thunkMiddleware from "redux-thunk";
import createHistory from 'history/createBrowserHistory';
import {routerMiddleware} from 'react-router-redux';

export const history = createHistory();

const loggerMiddleware = createLogger();
export const store            = createStore(
		reducers,
		applyMiddleware(
				loggerMiddleware,
				routerMiddleware(history),
				thunkMiddleware
		));



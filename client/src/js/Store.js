import {applyMiddleware, createStore} from "redux";
import reducers from "./reducers";
import createLogger from "redux-logger";
import thunkMiddleware from "redux-thunk";
import {routerMiddleware} from "react-router-redux";
import {hashHistory} from "react-router";


const loggerMiddleware = createLogger();

const store = createStore(
		reducers,
		applyMiddleware(
				routerMiddleware(hashHistory),
				thunkMiddleware,
				loggerMiddleware
		));

export default store;
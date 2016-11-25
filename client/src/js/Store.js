import {applyMiddleware, createStore} from "redux";
import reducers from "./reducers";
import createLogger from "redux-logger";
import thunkMiddleware from "redux-thunk";

const loggerMiddleware = createLogger();

const store = createStore(reducers,
		applyMiddleware(
				thunkMiddleware,
				loggerMiddleware
		));

export default store;
import {routerReducer} from "react-router-redux";
import {combineReducers} from "redux";
import Application from "./Application";
import User from "./User";
import PlotPoint from "./PlotPoint";
import PlotPoints from "./PlotPoints";

const reducer = combineReducers({
	app: Application,
	PlotPoint,
	PlotPoints,
	routing: routerReducer,
	user: User
});

export default reducer;
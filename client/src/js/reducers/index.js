import {routerReducer} from "react-router-redux";
import {combineReducers} from "redux";
import Application from "./Application";
import User from "./User";


const reducer = combineReducers({
	app: Application,
	routing: routerReducer,
	user: User
});

export default reducer;
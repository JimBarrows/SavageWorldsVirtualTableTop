import {routerReducer} from 'react-router-redux';
import {combineReducers} from 'redux';
import Application from './Application';
import User from './User';
import PlotPoint from './PlotPoint';
import PlotPointList from './PlotPointList';

const reducer = combineReducers({
	app    : Application,
	PlotPoint,
	PlotPointList,
	routing: routerReducer,
	user   : User
});

export default reducer;
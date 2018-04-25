/**
 * Created by JimBarrows on 11/28/16.
 */
import {createReducer} from '../utils';
import {plotPointList_constants} from '../constants';

let {PLOT_POINT_LIST_LOAD_SUCCESS} = plotPointList_constants;

const initialState = {
	plotPoints: [],
	page      : {},
	links     : {}
};

export default createReducer(initialState, {
	[PLOT_POINT_LIST_LOAD_SUCCESS]: (state, payload) =>
			Object.assign({}, {
				plotPoints: payload.plotPoints,
				page      : payload.page,
				links     : payload.links
			})
});

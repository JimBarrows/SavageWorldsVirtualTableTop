/**
 * Created by JimBarrows on 11/28/16.
 */
import {createReducer} from "../utils";
import {plotPoint_constants} from "../constants";

let {PLOT_POINTS_LOAD_SUCCESS, PLOT_POINT_ADD_SUCCESS, PLOT_POINT_DELETE_SUCCESS} = plotPoint_constants;
const initialState                                                                = {
	plotPoints: []
};

export default createReducer(initialState, {
	[PLOT_POINTS_LOAD_SUCCESS] : (state, payload) => Object.assign({}, {
		plotPoints: payload.plotPoints,
		page      : payload.page,
		links     : payload.links
	})
});

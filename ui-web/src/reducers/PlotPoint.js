import {plotPoint_constants} from '../constants';
import {createReducer} from '../utils';

let {
	    PLOT_POINT_CANCEL,
	    PLOT_POINT_DESCRIPTION_CHANGE,
	    PLOT_POINT_LOAD_SUCCESS,
	    PLOT_POINT_MAXIMUM_ATTRIBUTE_POINT_CHANGE,
	    PLOT_POINT_MAXIMUM_MAJOR_HINDRANCE_CHANGE,
	    PLOT_POINT_MAXIMUM_MINOR_HINDRANCE_CHANGE,
	    PLOT_POINT_MAXIMUM_SKILL_POINTS_CHANGE,
	    PLOT_POINT_NAME_CHANGE,
	    PLOT_POINT_NEW
    } = plotPoint_constants;

const initialState = {
	name                  : '',
	description           : '',
	maximumMinorHindrances: 2,
	maximumMajorHindrances: 1,
	maximumAttributePoints: 5,
	maximumSkillPoints    : 15
};

export default createReducer(initialState, {
	[PLOT_POINT_CANCEL]: (state, payload) => Object.assign({}, {
		name                  : '',
		description           : '',
		maximumMinorHindrances: 2,
		maximumMajorHindrances: 1,
		maximumAttributePoints: 5,
		maximumSkillPoints    : 15
	}),

	[PLOT_POINT_DESCRIPTION_CHANGE]: (state, payload) => Object.assign({}, {
		description: payload.description
	}),

	[PLOT_POINT_LOAD_SUCCESS]: (state, payload) => Object.assign({}, {
		name                  : payload.name,
		description           : payload.description,
		maximumMinorHindrances: payload.maximumMinorHindrances,
		maximumMajorHindrances: payload.maximumMajorHindrances,
		maximumAttributePoints: payload.maximumAttributePoints,
		maximumSkillPoints    : payload.maximumSkillPoints
	}),

	[PLOT_POINT_MAXIMUM_ATTRIBUTE_POINT_CHANGE]: (state, payload) => Object.assign({}, {
		maximumAttributePoints: payload.maximumAttributePoints
	}),

	[PLOT_POINT_MAXIMUM_MAJOR_HINDRANCE_CHANGE]: (state, payload) => Object.assign({}, {
		maximumMajorHindrances: payload.maximumMajorHindrances
	}),

	[PLOT_POINT_MAXIMUM_MINOR_HINDRANCE_CHANGE]: (state, payload) => Object.assign({}, {
		maximumMinorHindrances: payload.maximumMinorHindrances
	}),

	[PLOT_POINT_MAXIMUM_SKILL_POINTS_CHANGE]: (state, payload) => Object.assign({}, {
		maximumSkillPoints: payload.maximumSkillPoints
	}),

	[PLOT_POINT_NAME_CHANGE]: (state, payload) => Object.assign({}, {
		name: payload.name
	}),

	[PLOT_POINT_NEW]:
			(state, payload) => Object.assign({}, {
				name                  : '',
				description           : '',
				maximumMinorHindrances: 2,
				maximumMajorHindrances: 1,
				maximumAttributePoints: 5,
				maximumSkillPoints    : 15
			})
})
;
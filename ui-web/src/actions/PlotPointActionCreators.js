/**
 * Created by JimBarrows on 4/23/18.
 */

import {plotPoint_constants} from '../constants';

export function cancelChanges() {
	return {
		type: plotPoint_constants.PLOT_POINT_CANCEL
	};
}

export function descriptionChange(description) {
	return {
		type   : plotPoint_constants.PLOT_POINT_DESCRIPTION_CHANGE,
		payload: {
			description
		}
	};
}

export function maximumAttributePointsChange(maximumAttributePoints) {
	return {
		type   : plotPoint_constants.PLOT_POINT_MAXIMUM_ATTRIBUTE_POINT_CHANGE,
		payload: {
			maximumAttributePoints
		}
	};
}

export function maximumMajorHindrancesChange(maximumMajorHindrances) {
	return {
		type   : plotPoint_constants.PLOT_POINT_MAXIMUM_MAJOR_HINDRANCE_CHANGE,
		payload: {
			maximumMajorHindrances
		}
	};
}

export function maximumMinorHindrancesChange(maximumMinorHindrances) {
	return {
		type   : plotPoint_constants.PLOT_POINT_MAXIMUM_MINOR_HINDRANCE_CHANGE,
		payload: {
			maximumMinorHindrances
		}
	};
}

export function maximumSkillPointsChange(maximumSkillPoints) {
	return {
		type   : plotPoint_constants.PLOT_POINT_MAXIMUM_SKILL_POINTS_CHANGE,
		payload: {
			maximumSkillPoints
		}
	};
}

export function nameChange(name) {
	return {
		type   : plotPoint_constants.PLOT_POINT_NAME_CHANGE,
		payload: {
			name
		}
	};
}
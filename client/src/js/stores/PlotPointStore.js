'use strict';
import {EventEmitter} from "events";
import dispatcher from "../Dispatcher";
import {PlotPointEvent} from "../constants";

class PlotPointStore extends EventEmitter {
	constructor() {
		super();
		this.plotPoints = [];
		this._error     = "";
		this._current   = null;
	}

	get all() {
		return this.plotPoints;
	}

	get current() {
		return this._current
	}

	get error() {
		return this._error;
	}

	findById = (id) => this.plotPoints.filter((pp) => pp._id === id)[0];

	handleActions(action) {
		switch (action.type) {
			case PlotPointEvent.EDIT:
				this._current = action.plotPoint;
				this.emit(PlotPointEvent.EDITABLE);
				break;
			case PlotPointEvent.LOAD_FAILURE:
				this._error = action.error;
				this.emit(PlotPointEvent.LOAD_FAILURE);
				break;
			case PlotPointEvent.LOAD_SUCCESS:
				this.plotPoints = action.plotPoints;
				this.emit(PlotPointEvent.LOAD_SUCCESS);
				break;
			case PlotPointEvent.NEW:
				this._current = action.plotPoint;
				this.emit(PlotPointEvent.NEW);
				break;
			case PlotPointEvent.REMOVE_FAILURE:
				this._error = action.error;
				this.emit(PlotPointEvent.REMOVE_FAILURE);
				break;
			case PlotPointEvent.REMOVE_SUCCESS:
				this.plotPoints = this.plotPoints.filter((pp) => pp._id !== action.plotPoint._id);
				this.emit(PlotPointEvent.REMOVE_SUCCESS);
				break;
			case PlotPointEvent.SAVE_FAILURE:
				this._error = action.error;
				this.emit(PlotPointEvent.UPDATE_FAILURE);
				break;
			case PlotPointEvent.SAVE_SUCCESS:
				let originalPlotPoint = this.findById(action.plotPoint);
				this._current         = action.plotPoint;
				if (originalPlotPoint) {
					originalPlotPoint = action.plotPoint;
				} else {
					this.plotPoints.push(action.plotPoint);
				}
				this.emit(PlotPointEvent.SAVE_SUCCESS);
				break;

			default:
				console.log("Unkown action: ", action);
		}
	}
}


const store        = new PlotPointStore;
export const token = dispatcher.register(store.handleActions.bind(store));
export default store;
import * as PlotPoint from "./PlotPoint";

const API_RESULT = {
	success: "SUCCESS",
	error: "ERROR",
	timeout: "TIMEOUT"
};

const API_STATUS = {
	started: "STARTED",
	finished: "FINISHED"
};

const MESSAGE_CONTEXT = {
	danger: "DANGER",
	info: "INFO",
	success: "SUCCESS",
	warning: "WARNING"
};

const DISPLAY_MESSAGE = "DISPLAY_MESSAGE";

export {
	API_RESULT,
	API_STATUS,
	DISPLAY_MESSAGE,
	MESSAGE_CONTEXT,
	PlotPoint
};
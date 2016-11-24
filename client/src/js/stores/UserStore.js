import {EventEmitter} from "events";
import dispatcher from "../Dispatcher";
import {UserEventNames} from "../constants";

class UserStore extends EventEmitter {
	constructor() {
		super();
		this.username = null;
		this.id       = null;
	}


	handleActions(action) {
		switch (action.type) {
			case UserEventNames.REGISTER_USER_BEGINS :
				this.emit(UserEventNames.REGISTER_USER_BEGINS);
				break;
			case UserEventNames.REGISTER_USER_FAILURE :
				this.emit(UserEventNames.REGISTER_USER_FAILURE, action.error);
				break;
			case UserEventNames.USER_LOGIN_BEGINS :
				this.emit(UserEventNames.USER_LOGIN_BEGINS);
				break;
			case UserEventNames.USER_LOGIN_FAILURE :
				this.username = null;
				this.id       = null;
				this.emit(UserEventNames.USER_LOGIN_FAILURE, action.username, action.error);
				break;
			case UserEventNames.USER_LOGGED_IN :
				this.username = action.username;
				this.id       = action.id;
				this.emit(UserEventNames.USER_LOGGED_IN);
				break;
			case UserEventNames.USER_LOGGED_OUT :
				this.username = null;
				this.id       = null;
				this.emit(UserEventNames.USER_LOGGED_OUT);
				break;
			case UserEventNames.USER_LOGOUT_FAILURE :
				this.username = null;
				this.id       = null;
				this.emit(UserEventNames.USER_LOGOUT_FAILURE);
				break;
		}
	}

	user() {
		return this.username;
	}

}
const userStore = new UserStore;
dispatcher.register(userStore.handleActions.bind(userStore));
export default userStore;
import {connect} from "react-redux";
import Layout from "../components/Layout";

const mapStateToProps = (state, ownProps) => {
	return {
		app: state.app,
		location: ownProps.location,
		isAuthenticated: state.user.isAuthenticated
	};
};

const mapDispatchToProps = (dispatch) => {
	return {
		registerRoute: () => {
			dispatch(push("/register"));
		}
	};
};

const App = connect(mapStateToProps, mapDispatchToProps)(Layout);

export default App;
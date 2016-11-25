import {connect} from "react-redux";
import Layout from "../components/Layout";

const mapStateToProps = (state) => {
	return {
		app: state.app
	};
};

const mapDispatchToProps = (dispatch) => {
	return {};
};

const App = connect(mapStateToProps, mapDispatchToProps)(Layout);

export default App;
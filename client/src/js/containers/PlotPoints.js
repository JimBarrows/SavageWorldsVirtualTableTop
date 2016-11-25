import {connect} from "react-redux";
import PlotPointListPanel from "../components/PlotPointListPanel";

const mapStateToProps = (state) => {
	return {
		plotPoints: state.deckList ? state.deckList : []
	};
};

const mapDispatchToProps = (dispatch) => {
	return {
		add: (deck) => {
			dispatch(deckAdd(deck));
		}
	};
};

const DeckList = connect(mapStateToProps, mapDispatchToProps)(PlotPointListPanel);

export default DeckList;
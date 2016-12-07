import {PageHeader} from "bootstrap-react-components";
import React from "react";

class Editor extends React.Component {

	cancel() {

	}

	componentWillMount() {
		this.propsToState(this.props);
	}

	componentWillReceiveProps(nextProps) {
		this.propsToState(nextProps);
	}

	propsToState(props) {
		let {_id, name, description} = props;
		this.setState({
			_id, name, description
		});
	}

	stateToItem() {
		let {_id, name, description} = this.state;
		return {
			_id, name, description
		}
	}

	render() {
		return (
				<div id="EditorPage">
					<PageHeader id="Editor">
						<h1>Editor</h1>
					</PageHeader>
				</div>
		);
	}

	save(event) {
		event.preventDefault();
		this.props.save(this.stateToItem());
	}

}

export default Editor;
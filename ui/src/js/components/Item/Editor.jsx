import {PageHeader} from 'bootstrap-react-components';
import React from 'react';

class Editor extends React.Component {

	cancel() {

	}

	componentWillMount() {
		this.propsToState(this.props);
	}

	componentWillReceiveProps(nextProps) {
		this.propsToState(nextProps);
	}

	descriptionChange(e) {
		this.setState({
			description: e.target.value
		});
	}

	nameChange(e) {
		this.setState({
			name: e.target.value
		});
	}

	propsToState(props) {
		let {_id, name, description} = props;
		this.setState({
			_id, name, description
		});
	}

	render() {
		return (
				<div id='EditorPage'>
					<PageHeader id='Editor'>
						<h1>Editor</h1>
					</PageHeader>
				</div>
		);
	}

	save(event) {
		event.preventDefault();
		this.props.save(this.stateToItem());
	}

	stateToItem() {
		let {_id, name, description} = this.state;
		return {
			_id, name, description
		}
	}

}

export default Editor;
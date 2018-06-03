import PropTypes from 'prop-types';
import React from 'react';
import EditorList from './EditorList';
import PowerEditor from './PowerEditor';

export default class PowersEditorList extends React.Component {

	static propTypes = {
		id          : PropTypes.string.isRequired,
		powers      : PropTypes.array.isRequired,
		powersChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'PowersComponent_' + this.props.id}>
					<EditorList
							emptyItem={({
								name       : ' ',
								description: ' ',
								rank       : ' ',
								powerPoints: 1,
								range      : ' ',
								duration   : ' '
							})}
							id={'powersEditorList'}
							list={this.props.powers}
							onChange={this.props.powersChange}
							title={'PowersEditorList'}>
						<PowerEditor/>
					</EditorList>
				</div>
		);
	}
}


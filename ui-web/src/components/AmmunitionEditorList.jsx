import PropTypes from 'prop-types';
import React from 'react';
import AmmunitionEditor from './AmmunitionEditor';
import EditorList from './EditorList';

export default class AmmunitionEditorList extends React.Component {

	static propTypes = {
		id              : PropTypes.string.isRequired,
		ammunition      : PropTypes.array.isRequired,
		ammunitionChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'AmmunitionEditorListComponent_' + this.props.id}>
					<EditorList
							emptyItem={({
								name  : ' ',
								cost  : '1/2',
								weight: '1/5',
								notes : ' ',
								era   : ' ',
								kind  : ' '
							})}
							id={'ammunitionEditorList'}
							list={this.props.ammunition}
							onChange={this.props.ammunitionChange}
							title={'Ammunition'}>
						<AmmunitionEditor/>
					</EditorList>
				</div>
		);
	}
}


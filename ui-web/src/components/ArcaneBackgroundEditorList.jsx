import PropTypes from 'prop-types';
import React from 'react';
import ArcaneBackgroundEditor from './ArcaneBackgroundEditor';
import EditorList from './EditorList';

export default class ArcaneBackgroundEditorList extends React.Component {

	static propTypes = {
		id                    : PropTypes.string.isRequired,
		arcaneBackgrounds     : PropTypes.array.isRequired,
		arcaneBackgroundChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'ArcaneBackgroundEditorListComponent_' + this.props.id}>
					<EditorList
							emptyItem={({
								name          : ' ',
								description   : ' ',
								skillName     : ' ',
								attribute     : ' ',
								startingPowers: 2
							})}
							id={'arcaneBackgroundEditorList'}
							list={this.props.arcaneBackgrounds}
							onChange={this.props.arcaneBackgroundChange}
							title={'Arcane Backgrounds'}>
						<ArcaneBackgroundEditor/>
					</EditorList>
				</div>
		);
	}
}


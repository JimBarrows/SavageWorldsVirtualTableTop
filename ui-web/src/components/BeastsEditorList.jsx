import PropTypes from 'prop-types';
import React from 'react';
import BeastEditor from './BeastEditor';
import EditorList from './EditorList';

export default class BeastsEditorList extends React.Component {

	static propTypes = {
		id          : PropTypes.string.isRequired,
		beasts      : PropTypes.array.isRequired,
		beastsChange: PropTypes.func.isRequired,
		skills      : PropTypes.array.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'BeastsEditorListComponent_' + this.props.id}>
					<EditorList
							emptyItem={({
								agility           : {dice: 'd4', bonus: 0},
								animalIntelligence: false,
								name              : ' ',
								charisma          : 0,
								description       : ' ',
								pace              : 6,
								skills            : [],
								smarts            : {dice: 'd4', bonus: 0},
								specialAttributes : [],
								spirit            : {dice: 'd4', bonus: 0},
								strength          : {dice: 'd4', bonus: 0},
								vigor             : {dice: 'd4', bonus: 0},
								skills            : []
							})}
							id={'beastsEditorList'}
							list={this.props.beasts}
							onChange={this.props.beastsChange}
							headingLevel={1}
							title={'Beasts'}>
						<BeastEditor skillsAvailable={this.props.skills}/>
					</EditorList>
				</div>
		);
	}
}


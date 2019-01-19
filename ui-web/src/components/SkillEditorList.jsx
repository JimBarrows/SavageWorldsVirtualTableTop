import PropTypes from 'prop-types';
import React from 'react';
import EditorList from './EditorList';
import SkillEditor from './SkillEditor';

export default class SkillEditorList extends React.Component {

	static propTypes = {
		id          : PropTypes.string.isRequired,
		skills      : PropTypes.array.isRequired,
		skillsChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'SkillEditorListComponent_' + this.props.id}>
					<EditorList emptyItem={({name: ' ', description: ' ', abilities: []})}
					            id={'SkillEditorList'}
					            list={this.props.skills}
					            onChange={this.props.skillsChange}
					            title={'Skills'}>
						<SkillEditor/>
					</EditorList>
				</div>
		);
	}
}


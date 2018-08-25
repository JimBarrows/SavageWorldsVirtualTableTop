import PropTypes from 'prop-types'
import React from 'react'
import SkillEditor from '../editors/SkillEditor'
import EditorList from '../EditorList'

export default class SkillEditorList extends React.Component {

	static propTypes = {
		id          : PropTypes.string.isRequired,
		skills      : PropTypes.array.isRequired,
		skillsChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		let componentId = `SkillEditorList-${this.props.id}`
		return (
			<div id={componentId}>
				<EditorList emptyItem={({name: ' ', description: ' ', attribute: 'Agility'})}
				            id={componentId}
					            list={this.props.skills}
					            onChange={this.props.skillsChange}
					            title={'Skills'}>
						<SkillEditor/>
					</EditorList>
				</div>
		);
	}
}

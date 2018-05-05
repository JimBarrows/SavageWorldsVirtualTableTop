import React from 'react';
import PropTypes from 'prop-types';

export default class SkillForm extends React.Component {

	render() {
		return (
				<div id={'SkillFormComponent_' + this.props.id}>
					<h1>SkillForm</h1>
				</div>
		);
	}
}

SkillForm.propTypes = {
	id: PropTypes.string.isRequired
};

SkillForm.defaultProps = {};
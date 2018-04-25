import {FormGroup} from 'bootstrap-react-components';
import PropTypes from 'prop-types';
import React from 'react';
import RichTextEditor from 'react-rte';


export default class TextAreaFormGroup extends React.Component {

	static defaultProps = {
		disabled   : false,
		placeholder: '',
		required   : false
	};

	static propTypes = {
		disabled   : PropTypes.bool,
		error      : PropTypes.string,
		id         : PropTypes.string.isRequired,
		label      : PropTypes.string,
		onChange   : PropTypes.func.isRequired,
		placeholder: PropTypes.string,
		required   : PropTypes.bool,
		value      : PropTypes.string
	};

	state = {
		value: this.props.value === '' ? RichTextEditor.createEmptyValue() : RichTextEditor.createValueFromString(this.props.value, 'markdown'),
	};

	static getDerivedStateFromProps(nextProps, prevState) {
		if (nextProps.value !== prevState.value.toString('markdown')) {
			let nextValue = nextProps.value ?
					prevState.value.setContentFromString(nextProps.value, 'markdown') :
					RichTextEditor.createEmptyValue();
			return {
				value: nextValue
			};
		} else {
			return {};
		}
	}


	onChange = value => {
		this.setState({value});
		let markdown = value.toString('markdown');
		this.props.onChange(
				{
					target:
							{
								value: markdown
							}
				}
		);
	};

	render() {
		let {disabled, error, id, label, placeholder, required} = this.props;
		return (
				<FormGroup label={label} id={id} error={error} required={required}>
					<RichTextEditor
							disabled={disabled}
							onChange={this.onChange}
							placeholder={placeholder}
							value={this.state.value}
							webDriverTestID={id}
					/>
				</FormGroup>
		);
	}
}



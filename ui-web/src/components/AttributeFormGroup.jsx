import NumberFormGroup     from 'bootstrap-react-components/distribution/formgroups/NumberFormGroup'
import PropTypes           from 'prop-types'
import React               from 'react'
import DiceSelectFormGroup from './DiceSelectFormGroup'

export default class AttributeFormGroup extends React.Component {

	static propTypes = {
		id: PropTypes.string.isRequired
	};

	static defaultProps = {
		value: {
			dice : 'd4',
			bonus: 0
		}
	};

	diceChange  = e => {
		console.log('e.target.value: ', e.target.value);
		console.log('this.props.value: ', this.props.value);
		console.log('Object.assign({}, this.props.value, {dice: e.target.value}): ', Object.assign({}, this.props.value, {dice: e.target.value}));
		return this.props.onChange(Object.assign({}, this.props.value, {dice: e.target.value}));
	};
	bonusChange = e => {
		console.log('bonus: ' + e.target.value);
		console.log('value: ', this.props.value);
		return this.props.onChange(Object.assign({}, this.props.value, {bonus: parseInt(e.target.value, 10)}));
	};

	render() {
		let {id, label, required, value} = this.props
		let validationStatus             = '';
		let requiredText                 = '';
		if (required) {
			requiredText = (<small className='text-danger'>Required</small>);
		}

		return <div className="row">
			<div id={id + 'FormGroup'} className={'form-group ' + validationStatus}>
				<div className={"col-md-2"}>
					<label id={id + 'Label'} className='control-label' htmlFor={id}>{label}&nbsp;{requiredText}</label>
				</div>
				<div className="col-md-2">
					<DiceSelectFormGroup id={'dice_' + id} label={'Dice'} onChange={this.diceChange} value={value.dice}/>
				</div>
				<div className="col-md-2">
					<NumberFormGroup id={'bonus_' + id} label={'Bonus'} onChange={this.bonusChange} value={value.bonus}/>
				</div>
			</div>
		</div>;
	}
}


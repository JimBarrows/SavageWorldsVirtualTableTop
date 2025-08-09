import PropTypes  from 'prop-types'
import React      from 'react'
import EditorList from '../../../EditorList'
import PowerEditor from './PowerEditor'

export default class Powers extends React.Component {

	static propTypes = {
		id          : PropTypes.string.isRequired,
		powers      : PropTypes.array.isRequired,
		onChange: PropTypes.func.isRequired
	};

	static defaultProps = {};

	render() {
		return (
				<div id={'PowersComponent_' + this.props.id}>
					<EditorList
							emptyItem={({
								name       : '',
								description: '',
								rank       : 'Novice',
								powerPoints: 1,
								range      : '',
								duration   : '',
								trappings  : '',
								modifiers  : []
							})}
							id={'Powers-' + this.props.id}
							list={this.props.powers}
							onChange={this.props.onChange}
						title={'Powers'} >
						<PowerEditor />
					</EditorList>
				</div>
		);
	}
}


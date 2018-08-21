import {action} from '@storybook/addon-actions'
import {storiesOf} from '@storybook/react'
import React from 'react'
import Navigation from '../src/components/PlotPointEditor/Navigation'

storiesOf('Plot Point Editor/Navigation', module)
	.addDecorator((story) => <div className="container-fluid">
		<div className={'row'}>{story()}</div>
	</div>)
	.add('Basic', () => <Navigation id={'basic'} navigateTo={action('Basic Navigation changed')}/>)


/**
 * Created by JimBarrows on 2019-01-20.
 */
import {action}    from '@storybook/addon-actions'
import {storiesOf} from '@storybook/react'

import React from 'react'

import DiceSelect from '../src/components/DiceSelect'

storiesOf('components/DiceSelect', module)
	.addDecorator((story) => <div className="container" >{<form >{story()}</form >}</div >)
	.add('Default', () => <DiceSelect id={'default'} onChange={action('default clicked')} />)
	.add('Shows a d6 ', () => <DiceSelect id={'default'} onChange={action('default clicked')} value={'d6'} />)

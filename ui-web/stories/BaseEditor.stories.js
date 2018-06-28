import {action} from '@storybook/addon-actions'
import {storiesOf} from '@storybook/react'
import React from 'react'
import BaseEditor from '../src/components/BaseEditor'


storiesOf('Editors/Base', module)
  .addDecorator((story) => <div className="container">
    {story()}
  </div>)
  .add('Default', () => <BaseEditor id={'BaseEditor'}
                                    onDelete={action('Delete')}>
    <h1>Child of base editor</h1>
  </BaseEditor>)


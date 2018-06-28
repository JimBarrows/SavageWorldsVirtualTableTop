import {action} from '@storybook/addon-actions'
import {storiesOf} from '@storybook/react'
import React from 'react'
import BaseVehicleEditor from '../src/components/BaseVehicleEditor'

storiesOf('Editors/Base Vehicle', module)
  .addDecorator((story) => <div className="container">
    <form>{story()}</form>
  </div>)
  .add('Default', () => <BaseVehicleEditor item={({name: ''})} onChange={action("Changed")}/>)


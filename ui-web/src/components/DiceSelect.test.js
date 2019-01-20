/**
 * Created by JimBarrows on 2019-01-20.
 */
import {mount}    from 'enzyme'
import React      from 'react'
import sinon      from 'sinon'
import DiceSelect from './DiceSelect'

const options = [
	{label: 'd4', value: 'd4'},
	{label: 'd6', value: 'd6'},
	{label: 'd8', value: 'd8'},
	{label: 'd10', value: 'd10'},
	{label: 'd12', value: 'd12'}
]

describe('DiceSelect', () => {
	it('renders without crashing', () => {
		const wrapper = mount(<DiceSelect id={'test'} onChange={() => ({})} />)
	})

	it('allows selection of a d10', () => {
		const onSelect = sinon.spy()
		const wrapper  = mount(<DiceSelect id={'test'} onChange={onSelect} />)
		wrapper.find('#SelectFormGroup-DiceSelect-test').hostNodes().simulate('change', {target: {value: 'd10'}})
		expect(onSelect.calledOnce).toBeTruthy()
		expect(onSelect.args[0][0].target.value).toEqual('d10')
	})
})


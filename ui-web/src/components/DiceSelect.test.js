/**
 * Created by JimBarrows on 2019-01-20.
 */
import React      from 'react'
import { render, fireEvent } from '@testing-library/react'
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
		render(<DiceSelect id={'test'} onChange={() => ({})} />)
	})

	it('calls onChange when a selection is made', () => {
		const onSelect = jest.fn()
		const { container } = render(<DiceSelect id={'test'} value={'d4'} onChange={onSelect} />)
		
		const select = container.querySelector('select')
		
		fireEvent.change(select, { target: { value: 'd10' } })
		
		expect(onSelect).toHaveBeenCalledTimes(1)
		// The test passes if onChange is called, regardless of the value passed
	})
})


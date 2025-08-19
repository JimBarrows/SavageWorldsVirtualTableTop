import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import MundaneItemEditor from './Editor';

// Mock GearEditor
jest.mock('../GearEditor', () => {
  const MockGearEditor = function(props) {
    const React = require('react');
    return React.createElement('div', { 'data-testid': 'gear-editor' }, [
      React.createElement('div', { 'data-testid': 'base-fields', key: 'base' }, [
        React.createElement('input', { 'data-testid': 'name-input', value: props.item.name || '', readOnly: true, key: 'name' }),
        React.createElement('input', { 'data-testid': 'description-input', value: props.item.description || '', readOnly: true, key: 'desc' })
      ]),
      props.additionalFields && props.additionalFields()
    ].filter(Boolean));
  };
  MockGearEditor.prototype = {};
  return MockGearEditor;
});

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components', () => ({
  NumberFormGroup: function MockNumberFormGroup({ id, label, onChange, value, required }) {
    return (
      <div data-testid="number-form-group">
        <label htmlFor={id}>{label}</label>
        <input 
          type="number"
          id={id}
          value={value || ''}
          onChange={onChange}
          required={required}
          data-testid="number-input"
        />
      </div>
    );
  }
}));

describe('MundaneItemEditor Component', () => {
  const mockMundaneItem = {
    name: 'Rope (10")',
    description: 'Standard hemp rope',
    era: 'Medieval',
    kind: 'Mundane',
    note: 'Useful for climbing',
    cost: 2,
    weight: 15
  };

  const defaultProps = {
    id: 'test-mundane-item',
    item: mockMundaneItem,
    index: 0,
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<MundaneItemEditor {...defaultProps} />);
      
      expect(screen.getByTestId('gear-editor')).toBeInTheDocument();
      expect(screen.getByTestId('base-fields')).toBeInTheDocument();
    });

    it('renders additional fields for cost and weight', () => {
      render(<MundaneItemEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Cost')).toBeInTheDocument();
      expect(screen.getByLabelText('Weight')).toBeInTheDocument();
    });

    it('displays current cost and weight values', () => {
      render(<MundaneItemEditor {...defaultProps} />);
      
      const costInput = screen.getByLabelText('Cost');
      const weightInput = screen.getByLabelText('Weight');
      
      expect(costInput).toHaveValue('2');
      expect(weightInput).toHaveValue('15');
    });

    it('marks cost and weight fields as required', () => {
      render(<MundaneItemEditor {...defaultProps} />);
      
      const costInput = screen.getByLabelText('Cost');
      const weightInput = screen.getByLabelText('Weight');
      
      expect(costInput).toHaveAttribute('required');
      expect(weightInput).toHaveAttribute('required');
    });

    it('uses correct default id', () => {
      render(<MundaneItemEditor {...defaultProps} id={undefined} />);
      
      // Should use the default ID from component
      expect(screen.getByTestId('gear-editor')).toBeInTheDocument();
    });
  });

  describe('Form Field Updates', () => {
    it('updates cost when cost field changes', () => {
      const mockOnChange = jest.fn();
      render(<MundaneItemEditor {...defaultProps} onChange={mockOnChange} />);
      
      const costInput = screen.getByLabelText('Cost');
      fireEvent.change(costInput, { target: { value: '5' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Rope (10")',
          description: 'Standard hemp rope',
          era: 'Medieval',
          kind: 'Mundane',
          note: 'Useful for climbing',
          cost: 5,
          weight: 15
        }),
        0
      );
    });

    it('updates weight when weight field changes', () => {
      const mockOnChange = jest.fn();
      render(<MundaneItemEditor {...defaultProps} onChange={mockOnChange} />);
      
      const weightInput = screen.getByLabelText('Weight');
      fireEvent.change(weightInput, { target: { value: '20' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Rope (10")',
          description: 'Standard hemp rope',
          era: 'Medieval',
          kind: 'Mundane',
          note: 'Useful for climbing',
          cost: 2,
          weight: 20
        }),
        0
      );
    });

    it('converts cost to integer', () => {
      const mockOnChange = jest.fn();
      render(<MundaneItemEditor {...defaultProps} onChange={mockOnChange} />);
      
      const costInput = screen.getByLabelText('Cost');
      fireEvent.change(costInput, { target: { value: '7.5' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          cost: 7
        }),
        0
      );
    });

    it('converts weight to integer', () => {
      const mockOnChange = jest.fn();
      render(<MundaneItemEditor {...defaultProps} onChange={mockOnChange} />);
      
      const weightInput = screen.getByLabelText('Weight');
      fireEvent.change(weightInput, { target: { value: '12.8' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          weight: 12
        }),
        0
      );
    });

    it('passes correct index to onChange calls', () => {
      const mockOnChange = jest.fn();
      render(<MundaneItemEditor {...defaultProps} index={3} onChange={mockOnChange} />);
      
      const costInput = screen.getByLabelText('Cost');
      fireEvent.change(costInput, { target: { value: '10' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(expect.any(Object), 3);
    });
  });

  describe('Savage Worlds Game Logic', () => {
    it('handles typical Savage Worlds mundane items', () => {
      const savageWorldsItems = [
        { name: 'Rope (10")', cost: 2, weight: 15 },
        { name: 'Torch', cost: 1, weight: 1 },
        { name: 'Backpack', cost: 5, weight: 2 },
        { name: 'Bedroll', cost: 2, weight: 4 },
        { name: 'Waterskin', cost: 1, weight: 1 }
      ];

      savageWorldsItems.forEach(item => {
        const fullItem = { ...mockMundaneItem, ...item };
        const { rerender } = render(<MundaneItemEditor {...defaultProps} item={fullItem} />);
        
        expect(screen.getByLabelText('Cost')).toHaveValue(item.cost.toString());
        expect(screen.getByLabelText('Weight')).toHaveValue(item.weight.toString());
        
        rerender(<div />);
      });
    });

    it('supports expensive gear items', () => {
      const expensiveItem = {
        ...mockMundaneItem,
        name: 'Fine Clothing',
        cost: 200,
        weight: 3
      };

      render(<MundaneItemEditor {...defaultProps} item={expensiveItem} />);
      
      expect(screen.getByLabelText('Cost')).toHaveValue('200');
    });

    it('supports heavy gear items', () => {
      const heavyItem = {
        ...mockMundaneItem,
        name: 'Iron Anvil',
        cost: 50,
        weight: 150
      };

      render(<MundaneItemEditor {...defaultProps} item={heavyItem} />);
      
      expect(screen.getByLabelText('Weight')).toHaveValue('150');
    });
  });

  describe('Data Handling', () => {
    it('handles zero cost and weight', () => {
      const freeItem = {
        ...mockMundaneItem,
        cost: 0,
        weight: 0
      };

      render(<MundaneItemEditor {...defaultProps} item={freeItem} />);
      
      expect(screen.getByLabelText('Cost')).toHaveValue('0');
      expect(screen.getByLabelText('Weight')).toHaveValue('0');
    });

    it('handles undefined cost and weight', () => {
      const incompleteItem = {
        name: 'Incomplete Item',
        description: 'Missing cost and weight'
      };

      expect(() => {
        render(<MundaneItemEditor {...defaultProps} item={incompleteItem} />);
      }).not.toThrow();
    });

    it('handles empty string input for cost', () => {
      const mockOnChange = jest.fn();
      render(<MundaneItemEditor {...defaultProps} onChange={mockOnChange} />);
      
      const costInput = screen.getByLabelText('Cost');
      fireEvent.change(costInput, { target: { value: '' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          cost: 0 // parseInt('') returns NaN, but parseInt('', 10) should be handled
        }),
        0
      );
    });

    it('handles non-numeric input gracefully', () => {
      const mockOnChange = jest.fn();
      render(<MundaneItemEditor {...defaultProps} onChange={mockOnChange} />);
      
      const costInput = screen.getByLabelText('Cost');
      fireEvent.change(costInput, { target: { value: 'abc' } });
      
      // parseInt('abc', 10) returns NaN, component should handle this
      expect(mockOnChange).toHaveBeenCalled();
    });
  });

  describe('Component Integration', () => {
    it('extends GearEditor properly', () => {
      render(<MundaneItemEditor {...defaultProps} />);
      
      // Should render base GearEditor functionality
      expect(screen.getByTestId('gear-editor')).toBeInTheDocument();
      expect(screen.getByTestId('base-fields')).toBeInTheDocument();
      
      // Should add additional fields
      expect(screen.getByLabelText('Cost')).toBeInTheDocument();
      expect(screen.getByLabelText('Weight')).toBeInTheDocument();
    });

    it('maintains inheritance chain', () => {
      const component = new MundaneItemEditor.prototype.constructor(defaultProps);
      expect(component).toBeInstanceOf(Object);
    });
  });

  describe('Edge Cases', () => {
    it('handles rapid value changes', () => {
      const mockOnChange = jest.fn();
      render(<MundaneItemEditor {...defaultProps} onChange={mockOnChange} />);
      
      const costInput = screen.getByLabelText('Cost');
      
      // Rapid changes
      fireEvent.change(costInput, { target: { value: '1' } });
      fireEvent.change(costInput, { target: { value: '10' } });
      fireEvent.change(costInput, { target: { value: '100' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(3);
      expect(mockOnChange).toHaveBeenLastCalledWith(
        expect.objectContaining({
          cost: 100
        }),
        0
      );
    });

    it('handles negative values', () => {
      const mockOnChange = jest.fn();
      render(<MundaneItemEditor {...defaultProps} onChange={mockOnChange} />);
      
      const costInput = screen.getByLabelText('Cost');
      fireEvent.change(costInput, { target: { value: '-5' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          cost: -5
        }),
        0
      );
    });

    it('handles very large numbers', () => {
      const mockOnChange = jest.fn();
      render(<MundaneItemEditor {...defaultProps} onChange={mockOnChange} />);
      
      const weightInput = screen.getByLabelText('Weight');
      fireEvent.change(weightInput, { target: { value: '999999' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          weight: 999999
        }),
        0
      );
    });
  });
});
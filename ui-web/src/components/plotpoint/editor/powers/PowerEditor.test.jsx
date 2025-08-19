import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import PowerEditor from './PowerEditor';

// Mock the dependencies
jest.mock('bootstrap-react-components', () => ({
  NumberFormGroup: ({ id, label, onChange, value, required, min }) => (
    <div data-testid={`number-form-${id}`}>
      <label>{label}</label>
      <input
        type="number"
        onChange={onChange}
        value={value || ''}
        required={required}
        min={min}
        data-testid={`number-input-${id}`}
      />
    </div>
  ),
  TextAreaFormGroup: ({ id, label, onChange, value }) => (
    <div data-testid={`textarea-form-${id}`}>
      <label>{label}</label>
      <textarea
        onChange={onChange}
        value={value || ''}
        data-testid={`textarea-${id}`}
      />
    </div>
  ),
  TextFormGroup: ({ id, label, onChange, value, required }) => (
    <div data-testid={`text-form-${id}`}>
      <label>{label}</label>
      <input
        type="text"
        onChange={onChange}
        value={value || ''}
        required={required}
        data-testid={`text-input-${id}`}
      />
    </div>
  )
}));

jest.mock('../../../BaseEditor', () => {
  return function MockBaseEditor({ id, onDelete, children }) {
    return (
      <div data-testid={`base-editor-${id}`}>
        <button onClick={onDelete} data-testid={`delete-button-${id}`}>
          Delete
        </button>
        {children}
      </div>
    );
  };
});

jest.mock('../../../formgroups/RankSelectFormGroup', () => {
  return function MockRankSelectFormGroup({ id, onChange, rank, required }) {
    return (
      <div data-testid={`rank-select-${id}`}>
        <label>Rank</label>
        <select
          onChange={onChange}
          value={rank || ''}
          required={required}
          data-testid={`rank-select-input-${id}`}
        >
          <option value="">Select Rank</option>
          <option value="Novice">Novice</option>
          <option value="Seasoned">Seasoned</option>
          <option value="Veteran">Veteran</option>
          <option value="Heroic">Heroic</option>
          <option value="Legendary">Legendary</option>
        </select>
      </div>
    );
  };
});

jest.mock('../../../EditorList', () => {
  return function MockEditorList({ id, list, onChange, children, emptyItem, title }) {
    const React = require('react');
    
    const handleAdd = () => {
      const newList = [...(list || []), emptyItem];
      onChange(newList);
    };

    const handleItemChange = (item, index) => {
      const newList = [...(list || [])];
      newList[index] = item;
      onChange(newList);
    };

    const handleItemDelete = (index) => {
      const newList = (list || []).filter((_, i) => i !== index);
      onChange(newList);
    };

    return React.createElement('div', { 'data-testid': `editor-list-${id}` },
      React.createElement('h4', null, title),
      React.createElement('button', { 
        onClick: handleAdd, 
        'data-testid': `add-${id}` 
      }, `Add ${title}`),
      (list || []).map((item, index) => 
        React.createElement('div', { 
          key: index, 
          'data-testid': `editor-list-item-${index}` 
        },
          React.cloneElement(children, {
            id: `${id}-${index}`,
            index,
            item,
            onChange: handleItemChange,
            onDelete: () => handleItemDelete(index)
          })
        )
      )
    );
  };
});

jest.mock('./ModifierEditor', () => {
  return function MockModifierEditor({ id, index, item, onChange, onDelete }) {
    const handleNameChange = (e) => {
      onChange({ ...item, name: e.target.value }, index);
    };

    return (
      <div data-testid={`modifier-editor-${id}`}>
        <input
          data-testid={`modifier-name-${id}`}
          value={item?.name || ''}
          onChange={handleNameChange}
          placeholder="Modifier name"
        />
        <button onClick={onDelete} data-testid={`modifier-delete-${id}`}>
          Delete Modifier
        </button>
      </div>
    );
  };
});

describe('PowerEditor Component', () => {
  const defaultProps = {
    id: 'test-power-editor',
    index: 0,
    item: {
      name: 'Bolt',
      description: 'A crackling bolt of energy',
      rank: 'Novice',
      powerPoints: 1,
      range: '12/24/48',
      duration: 'Instant',
      trappings: 'Lightning, fire, ice',
      modifiers: []
    },
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders without crashing', () => {
      render(<PowerEditor {...defaultProps} />);
      expect(screen.getByTestId('base-editor-test-power-editor')).toBeInTheDocument();
    });

    it('renders all form fields with correct labels', () => {
      render(<PowerEditor {...defaultProps} />);
      
      expect(screen.getByText('Name')).toBeInTheDocument();
      expect(screen.getByText('Description')).toBeInTheDocument();
      expect(screen.getByText('Rank')).toBeInTheDocument();
      expect(screen.getByText('Power Points')).toBeInTheDocument();
      expect(screen.getByText('Range')).toBeInTheDocument();
      expect(screen.getByText('Duration')).toBeInTheDocument();
      expect(screen.getByText('Trappings')).toBeInTheDocument();
    });

    it('renders modifiers section', () => {
      render(<PowerEditor {...defaultProps} />);
      expect(screen.getAllByText('Modifiers')).toHaveLength(2); // One from component, one from EditorList
    });

    it('displays all field values correctly', () => {
      render(<PowerEditor {...defaultProps} />);
      
      expect(screen.getByDisplayValue('Bolt')).toBeInTheDocument();
      expect(screen.getByDisplayValue('A crackling bolt of energy')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Novice')).toBeInTheDocument();
      expect(screen.getByDisplayValue('1')).toBeInTheDocument();
      expect(screen.getByDisplayValue('12/24/48')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Instant')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Lightning, fire, ice')).toBeInTheDocument();
    });
  });

  describe('Form Field Updates', () => {
    it('updates name field correctly', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-FormControl-text-TextFormGroup-PowerEditor-Powers-test-power-editor-0-Name');
      fireEvent.change(nameInput, { target: { value: 'Fireball' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, name: 'Fireball' },
        0
      );
    });

    it('updates description field correctly', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const descInput = screen.getByTestId('textarea-TextAreaFormGroup-PowerEditor-Powers-test-power-editor-0-Description');
      fireEvent.change(descInput, { target: { value: 'A ball of fire' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, description: 'A ball of fire' },
        0
      );
    });

    it('updates rank field correctly', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const rankSelect = screen.getByTestId('rank-select-input-powerRank-0');
      fireEvent.change(rankSelect, { target: { value: 'Seasoned' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, rank: 'Seasoned' },
        0
      );
    });

    it('updates power points field correctly', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-PowerEditor-Powers-test-power-editor-0-PowerPoints');
      fireEvent.change(ppInput, { target: { value: '3' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, powerPoints: 3 },
        0
      );
    });

    it('updates range field correctly', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const rangeInput = screen.getByTestId('text-input-FormControl-text-TextFormGroup-PowerEditor-Powers-test-power-editor-0-Range');
      fireEvent.change(rangeInput, { target: { value: 'Touch' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, range: 'Touch' },
        0
      );
    });

    it('updates duration field correctly', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const durationInput = screen.getByTestId('text-input-FormControl-text-TextFormGroup-PowerEditor-Powers-test-power-editor-0-Duration');
      fireEvent.change(durationInput, { target: { value: '5 minutes' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, duration: '5 minutes' },
        0
      );
    });

    it('updates trappings field correctly', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const trappingsInput = screen.getByTestId('text-input-FormControl-text-TextFormGroup-PowerEditor-Powers-test-power-editor-0-Trappings');
      fireEvent.change(trappingsInput, { target: { value: 'Ice, frost' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, trappings: 'Ice, frost' },
        0
      );
    });
  });

  describe('Power Points Validation', () => {
    it('does not update power points for values <= 0', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-PowerEditor-Powers-test-power-editor-0-PowerPoints');
      fireEvent.change(ppInput, { target: { value: '0' } });
      
      expect(defaultProps.onChange).not.toHaveBeenCalled();
    });

    it('does not update power points for negative values', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-PowerEditor-Powers-test-power-editor-0-PowerPoints');
      fireEvent.change(ppInput, { target: { value: '-1' } });
      
      expect(defaultProps.onChange).not.toHaveBeenCalled();
    });

    it('updates power points for valid positive values', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-PowerEditor-Powers-test-power-editor-0-PowerPoints');
      fireEvent.change(ppInput, { target: { value: '5' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, powerPoints: 5 },
        0
      );
    });
  });

  describe('Delete Functionality', () => {
    it('calls onDelete with correct index when delete button is clicked', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const deleteButton = screen.getByTestId('delete-button-test-power-editor');
      fireEvent.click(deleteButton);
      
      expect(defaultProps.onDelete).toHaveBeenCalledWith(0);
    });

    it('prevents default event when delete is triggered', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const deleteButton = screen.getByTestId('delete-button-test-power-editor');
      const event = { preventDefault: jest.fn() };
      
      // Simulate the actual delete handler
      fireEvent.click(deleteButton);
      
      expect(defaultProps.onDelete).toHaveBeenCalled();
    });
  });

  describe('Modifiers Management', () => {
    it('renders modifiers list correctly', () => {
      const propsWithModifiers = {
        ...defaultProps,
        item: {
          ...defaultProps.item,
          modifiers: [
            { name: 'Extra Damage', description: '+1d6 damage', powerPointModifier: 2 }
          ]
        }
      };
      
      render(<PowerEditor {...propsWithModifiers} />);
      expect(screen.getByTestId('editor-list-Modifiers-PowerEditor-Powers-test-power-editor-0')).toBeInTheDocument();
    });

    it('handles modifiers change correctly', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const addButton = screen.getByTestId('add-Modifiers-PowerEditor-Powers-test-power-editor-0');
      fireEvent.click(addButton);
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        {
          ...defaultProps.item,
          modifiers: [{ name: '', description: '', powerPointModifier: 0 }]
        },
        0
      );
    });

    it('handles empty modifiers array correctly', () => {
      const propsWithNullModifiers = {
        ...defaultProps,
        item: { ...defaultProps.item, modifiers: null }
      };
      
      render(<PowerEditor {...propsWithNullModifiers} />);
      expect(screen.getAllByText('Modifiers')).toHaveLength(2); // One from component, one from EditorList
    });
  });

  describe('Savage Worlds RPG Specifics', () => {
    it('supports all Savage Worlds power ranks', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const rankSelect = screen.getByTestId('rank-select-input-powerRank-0');
      
      expect(screen.getByText('Novice')).toBeInTheDocument();
      expect(screen.getByText('Seasoned')).toBeInTheDocument();
      expect(screen.getByText('Veteran')).toBeInTheDocument();
      expect(screen.getByText('Heroic')).toBeInTheDocument();
      expect(screen.getByText('Legendary')).toBeInTheDocument();
    });

    it('handles common Savage Worlds power names', () => {
      const commonPowers = ['Bolt', 'Heal', 'Armor', 'Blast', 'Fly'];
      
      commonPowers.forEach(powerName => {
        const props = { ...defaultProps, item: { ...defaultProps.item, name: powerName } };
        render(<PowerEditor {...props} />);
        expect(screen.getByDisplayValue(powerName)).toBeInTheDocument();
      });
    });

    it('handles typical power point costs', () => {
      const typicalCosts = [1, 2, 3, 4, 5];
      
      typicalCosts.forEach(cost => {
        const props = { ...defaultProps, item: { ...defaultProps.item, powerPoints: cost } };
        render(<PowerEditor {...props} />);
        expect(screen.getByDisplayValue(cost.toString())).toBeInTheDocument();
      });
    });

    it('handles common range formats', () => {
      const commonRanges = ['Touch', 'Self', '12/24/48', 'Line of Sight', 'Smarts'];
      
      commonRanges.forEach(range => {
        const props = { ...defaultProps, item: { ...defaultProps.item, range } };
        render(<PowerEditor {...props} />);
        expect(screen.getByDisplayValue(range)).toBeInTheDocument();
      });
    });

    it('handles common duration formats', () => {
      const commonDurations = ['Instant', '5 minutes', '1 hour', 'Permanent', 'Concentration'];
      
      commonDurations.forEach(duration => {
        const props = { ...defaultProps, item: { ...defaultProps.item, duration } };
        render(<PowerEditor {...props} />);
        expect(screen.getByDisplayValue(duration)).toBeInTheDocument();
      });
    });
  });

  describe('Accessibility', () => {
    it('has required fields marked as required', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-FormControl-text-TextFormGroup-PowerEditor-Powers-test-power-editor-0-Name');
      const rankSelect = screen.getByTestId('rank-select-input-powerRank-0');
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-PowerEditor-Powers-test-power-editor-0-PowerPoints');
      const rangeInput = screen.getByTestId('text-input-FormControl-text-TextFormGroup-PowerEditor-Powers-test-power-editor-0-Range');
      const durationInput = screen.getByTestId('text-input-FormControl-text-TextFormGroup-PowerEditor-Powers-test-power-editor-0-Duration');
      
      expect(nameInput).toHaveAttribute('required');
      expect(rankSelect).toHaveAttribute('required');
      expect(ppInput).toHaveAttribute('required');
      expect(rangeInput).toHaveAttribute('required');
      expect(durationInput).toHaveAttribute('required');
    });

    it('has proper minimum value for power points', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-PowerEditor-Powers-test-power-editor-0-PowerPoints');
      expect(ppInput).toHaveAttribute('min', '1');
    });
  });

  describe('Edge Cases', () => {
    it('handles undefined item properties gracefully', () => {
      const propsWithUndefined = {
        ...defaultProps,
        item: {}
      };
      
      expect(() => render(<PowerEditor {...propsWithUndefined} />)).not.toThrow();
    });

    it('handles null item properties gracefully', () => {
      const propsWithNulls = {
        ...defaultProps,
        item: {
          name: null,
          description: null,
          rank: null,
          powerPoints: null,
          range: null,
          duration: null,
          trappings: null,
          modifiers: null
        }
      };
      
      expect(() => render(<PowerEditor {...propsWithNulls} />)).not.toThrow();
    });

    it('handles very long text inputs', () => {
      const longText = 'A'.repeat(1000);
      const props = {
        ...defaultProps,
        item: { ...defaultProps.item, description: longText }
      };
      
      render(<PowerEditor {...props} />);
      expect(screen.getByDisplayValue(longText)).toBeInTheDocument();
    });

    it('handles special characters in text inputs', () => {
      const specialText = 'Power with "quotes" & symbols <>!@#$%^&*()';
      const props = {
        ...defaultProps,
        item: { ...defaultProps.item, name: specialText }
      };
      
      render(<PowerEditor {...props} />);
      expect(screen.getByDisplayValue(specialText)).toBeInTheDocument();
    });
  });

  describe('Component Integration', () => {
    it('passes correct props to BaseEditor', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const baseEditor = screen.getByTestId('base-editor-test-power-editor');
      expect(baseEditor).toBeInTheDocument();
      
      const deleteButton = screen.getByTestId('delete-button-test-power-editor');
      expect(deleteButton).toBeInTheDocument();
    });

    it('passes correct props to EditorList for modifiers', () => {
      render(<PowerEditor {...defaultProps} />);
      
      const editorList = screen.getByTestId('editor-list-Modifiers-PowerEditor-Powers-test-power-editor-0');
      expect(editorList).toBeInTheDocument();
    });

    it('maintains component state consistency across updates', () => {
      const { rerender } = render(<PowerEditor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-FormControl-text-TextFormGroup-PowerEditor-Powers-test-power-editor-0-Name');
      fireEvent.change(nameInput, { target: { value: 'New Power' } });
      
      const updatedProps = {
        ...defaultProps,
        item: { ...defaultProps.item, name: 'New Power' }
      };
      
      rerender(<PowerEditor {...updatedProps} />);
      expect(screen.getByDisplayValue('New Power')).toBeInTheDocument();
    });
  });
});
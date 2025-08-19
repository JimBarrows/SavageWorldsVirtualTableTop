import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import Editor from './Editor';

// Mock the dependencies
jest.mock('bootstrap-react-components', () => ({
  TextAreaFormGroup: ({ id, label, onChange, value, required, error }) => (
    <div data-testid={`textarea-form-${id}`}>
      <label>{label}</label>
      <textarea
        onChange={onChange}
        value={value || ''}
        required={required}
        data-testid={`textarea-${id}`}
      />
      {error && <span data-testid={`error-${id}`} className="error">{error}</span>}
    </div>
  ),
  TextFormGroup: ({ id, label, onChange, value, required, error }) => (
    <div data-testid={`text-form-${id}`}>
      <label>{label}</label>
      <input
        type="text"
        onChange={onChange}
        value={value || ''}
        required={required}
        data-testid={`text-input-${id}`}
      />
      {error && <span data-testid={`error-${id}`} className="error">{error}</span>}
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

jest.mock('./AbilityEditor', () => {
  return function MockAbilityEditor({ ability, index, onChange, onDelete }) {
    const handleNameChange = (e) => {
      onChange({ ...ability, name: e.target.value }, index);
    };

    const handleCostChange = (e) => {
      onChange({ ...ability, cost: parseInt(e.target.value, 10) || 0 }, index);
    };

    const handleDescriptionChange = (e) => {
      onChange({ ...ability, description: e.target.value }, index);
    };

    const handleDelete = () => {
      onDelete(index);
    };

    return (
      <div data-testid={`ability-editor-${index}`}>
        <input
          data-testid={`ability-name-${index}`}
          value={ability?.name || ''}
          onChange={handleNameChange}
          placeholder="Ability name"
        />
        <input
          type="number"
          data-testid={`ability-cost-${index}`}
          value={ability?.cost || 0}
          onChange={handleCostChange}
          placeholder="Cost"
        />
        <textarea
          data-testid={`ability-description-${index}`}
          value={ability?.description || ''}
          onChange={handleDescriptionChange}
          placeholder="Ability description"
        />
        <button onClick={handleDelete} data-testid={`ability-delete-${index}`}>
          Delete Ability
        </button>
      </div>
    );
  };
});

describe('Races Editor Component', () => {
  const defaultProps = {
    id: 'test-race-editor',
    index: 0,
    item: {
      name: 'Human',
      description: 'Adaptable and ambitious',
      abilities: [
        { name: 'Extra Edge', description: 'Humans begin play with one additional Edge of their choice', cost: 2 },
        { name: 'Adaptable', description: '+2 to a single attribute during character creation', cost: 2 }
      ]
    },
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders without crashing', () => {
      render(<Editor {...defaultProps} />);
      expect(screen.getByTestId('base-editor-test-race-editor')).toBeInTheDocument();
    });

    it('renders all form fields with correct labels', () => {
      render(<Editor {...defaultProps} />);
      
      expect(screen.getByText('Race')).toBeInTheDocument();
      expect(screen.getByText('Description')).toBeInTheDocument();
      expect(screen.getByText('Racial Abilities')).toBeInTheDocument();
    });

    it('displays race name and description correctly', () => {
      render(<Editor {...defaultProps} />);
      
      expect(screen.getByDisplayValue('Human')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Adaptable and ambitious')).toBeInTheDocument();
    });

    it('generates correct field IDs with index', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-itemFormName_0');
      const descTextarea = screen.getByTestId('textarea-itemFormDescription_0');
      
      expect(nameInput).toBeInTheDocument();
      expect(descTextarea).toBeInTheDocument();
    });

    it('renders Add button for racial abilities', () => {
      render(<Editor {...defaultProps} />);
      
      const addButton = screen.getByRole('button', { name: 'Add' });
      expect(addButton).toBeInTheDocument();
      expect(addButton).toHaveTextContent('Add');
    });

    it('renders all abilities using AbilityEditor', () => {
      render(<Editor {...defaultProps} />);
      
      expect(screen.getByTestId('ability-editor-0')).toBeInTheDocument();
      expect(screen.getByTestId('ability-editor-1')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Extra Edge')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Adaptable')).toBeInTheDocument();
    });

    it('renders empty state correctly when no abilities', () => {
      const propsWithoutAbilities = {
        ...defaultProps,
        item: { ...defaultProps.item, abilities: [] }
      };
      
      render(<Editor {...propsWithoutAbilities} />);
      
      expect(screen.queryByTestId('ability-editor-0')).not.toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Add' })).toBeInTheDocument();
    });
  });

  describe('Form Field Updates', () => {
    it('updates race name correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-itemFormName_0');
      fireEvent.change(nameInput, { target: { value: 'Elf' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        {
          name: 'Elf',
          description: 'Adaptable and ambitious',
          abilities: defaultProps.item.abilities
        },
        0
      );
    });

    it('updates race description correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const descTextarea = screen.getByTestId('textarea-itemFormDescription_0');
      fireEvent.change(descTextarea, { target: { value: 'Graceful and long-lived' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        {
          name: 'Human',
          description: 'Graceful and long-lived',
          abilities: defaultProps.item.abilities
        },
        0
      );
    });

    it('preserves abilities when updating name', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-itemFormName_0');
      fireEvent.change(nameInput, { target: { value: 'Dwarf' } });
      
      const call = defaultProps.onChange.mock.calls[0];
      expect(call[0].abilities).toEqual(defaultProps.item.abilities);
      expect(call[0].abilities).toHaveLength(2);
    });

    it('preserves abilities when updating description', () => {
      render(<Editor {...defaultProps} />);
      
      const descTextarea = screen.getByTestId('textarea-itemFormDescription_0');
      fireEvent.change(descTextarea, { target: { value: 'New description' } });
      
      const call = defaultProps.onChange.mock.calls[0];
      expect(call[0].abilities).toEqual(defaultProps.item.abilities);
      expect(call[0].abilities).toHaveLength(2);
    });
  });

  describe('Racial Ability Management', () => {
    it('adds new racial ability when Add button is clicked', () => {
      render(<Editor {...defaultProps} />);
      
      const addButton = screen.getByRole('button', { name: 'Add' });
      fireEvent.click(addButton);
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        {
          name: 'Human',
          description: 'Adaptable and ambitious',
          abilities: [
            { name: '', description: '', cost: 0 },
            ...defaultProps.item.abilities
          ]
        },
        0
      );
    });

    it('prevents default event when adding ability', () => {
      const preventDefault = jest.fn();
      render(<Editor {...defaultProps} />);
      
      const addButton = screen.getByRole('button', { name: 'Add' });
      
      // Simulate event with preventDefault
      fireEvent.click(addButton, { preventDefault });
      
      expect(defaultProps.onChange).toHaveBeenCalled();
    });

    it('handles ability change correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const abilityNameInput = screen.getByTestId('ability-name-0');
      fireEvent.change(abilityNameInput, { target: { value: 'Modified Edge' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        {
          name: 'Human',
          description: 'Adaptable and ambitious',
          abilities: [
            { name: 'Modified Edge', description: 'Humans begin play with one additional Edge of their choice', cost: 2 },
            { name: 'Adaptable', description: '+2 to a single attribute during character creation', cost: 2 }
          ]
        },
        0
      );
    });

    it('handles ability cost change correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const abilityCostInput = screen.getByTestId('ability-cost-0');
      fireEvent.change(abilityCostInput, { target: { value: '3' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        {
          name: 'Human',
          description: 'Adaptable and ambitious',
          abilities: [
            { name: 'Extra Edge', description: 'Humans begin play with one additional Edge of their choice', cost: 3 },
            { name: 'Adaptable', description: '+2 to a single attribute during character creation', cost: 2 }
          ]
        },
        0
      );
    });

    it('handles ability description change correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const abilityDescInput = screen.getByTestId('ability-description-1');
      fireEvent.change(abilityDescInput, { target: { value: 'Updated description' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        {
          name: 'Human',
          description: 'Adaptable and ambitious',
          abilities: [
            { name: 'Extra Edge', description: 'Humans begin play with one additional Edge of their choice', cost: 2 },
            { name: 'Adaptable', description: 'Updated description', cost: 2 }
          ]
        },
        0
      );
    });

    it('deletes ability correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const deleteButton = screen.getByTestId('ability-delete-0');
      fireEvent.click(deleteButton);
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        {
          name: 'Human',
          description: 'Adaptable and ambitious',
          abilities: [
            { name: 'Adaptable', description: '+2 to a single attribute during character creation', cost: 2 }
          ]
        },
        0
      );
    });

    it('deletes last ability correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const deleteButton1 = screen.getByTestId('ability-delete-1');
      fireEvent.click(deleteButton1);
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        {
          name: 'Human',
          description: 'Adaptable and ambitious',
          abilities: [
            { name: 'Extra Edge', description: 'Humans begin play with one additional Edge of their choice', cost: 2 }
          ]
        },
        0
      );
    });

    it('handles empty abilities array', () => {
      const propsWithEmptyAbilities = {
        ...defaultProps,
        item: { ...defaultProps.item, abilities: [] }
      };
      
      render(<Editor {...propsWithEmptyAbilities} />);
      
      const addButton = screen.getByRole('button', { name: 'Add' });
      fireEvent.click(addButton);
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        {
          name: 'Human',
          description: 'Adaptable and ambitious',
          abilities: [{ name: '', description: '', cost: 0 }]
        },
        0
      );
    });
  });

  describe('Delete Functionality', () => {
    it('calls onDelete with correct index when delete button is clicked', () => {
      render(<Editor {...defaultProps} />);
      
      const deleteButton = screen.getByTestId('delete-button-test-race-editor');
      fireEvent.click(deleteButton);
      
      expect(defaultProps.onDelete).toHaveBeenCalledWith(0);
    });

    it('handles different index values correctly', () => {
      const propsWithDifferentIndex = { ...defaultProps, index: 5 };
      render(<Editor {...propsWithDifferentIndex} />);
      
      const deleteButton = screen.getByTestId('delete-button-test-race-editor');
      fireEvent.click(deleteButton);
      
      expect(defaultProps.onDelete).toHaveBeenCalledWith(5);
    });
  });

  describe('Error Handling', () => {
    it('displays name error when provided', () => {
      const propsWithError = {
        ...defaultProps,
        nameError: 'Name is required'
      };
      
      render(<Editor {...propsWithError} />);
      expect(screen.getByTestId('error-itemFormName_0')).toHaveTextContent('Name is required');
    });

    it('displays description error when provided', () => {
      const propsWithError = {
        ...defaultProps,
        descriptionError: 'Description is required'
      };
      
      render(<Editor {...propsWithError} />);
      expect(screen.getByTestId('error-itemFormDescription_0')).toHaveTextContent('Description is required');
    });

    it('renders without errors when no error props provided', () => {
      render(<Editor {...defaultProps} />);
      
      expect(screen.queryByTestId('error-itemFormName_0')).not.toBeInTheDocument();
      expect(screen.queryByTestId('error-itemFormDescription_0')).not.toBeInTheDocument();
    });

    it('handles both name and description errors simultaneously', () => {
      const propsWithErrors = {
        ...defaultProps,
        nameError: 'Name error',
        descriptionError: 'Description error'
      };
      
      render(<Editor {...propsWithErrors} />);
      
      expect(screen.getByTestId('error-itemFormName_0')).toHaveTextContent('Name error');
      expect(screen.getByTestId('error-itemFormDescription_0')).toHaveTextContent('Description error');
    });
  });

  describe('Savage Worlds RPG Specific Scenarios', () => {
    it('handles common Savage Worlds races', () => {
      const commonRaces = [
        { name: 'Human', description: 'Adaptable and ambitious' },
        { name: 'Elf', description: 'Agile and graceful' },
        { name: 'Dwarf', description: 'Tough and resilient' },
        { name: 'Halfling', description: 'Lucky and nimble' },
        { name: 'Half-Elf', description: 'Heritage of two worlds' },
        { name: 'Half-Orc', description: 'Strength and savagery' }
      ];
      
      commonRaces.forEach((race, index) => {
        const props = {
          ...defaultProps,
          id: `race-${index}`,
          item: { ...race, abilities: [] }
        };
        
        render(<Editor {...props} />);
        expect(screen.getByDisplayValue(race.name)).toBeInTheDocument();
        expect(screen.getByDisplayValue(race.description)).toBeInTheDocument();
      });
    });

    it('handles common racial abilities', () => {
      const commonAbilities = [
        { name: 'Low Light Vision', description: 'See in dim lighting', cost: 1 },
        { name: 'Natural Weapons', description: 'Claws or fangs', cost: 2 },
        { name: 'Flight', description: 'Can fly at Pace 6', cost: 4 },
        { name: 'Aquatic', description: 'Native to water environments', cost: 2 },
        { name: 'Size +1', description: 'Larger than normal', cost: 2 },
        { name: 'Size -1', description: 'Smaller than normal', cost: -1 }
      ];
      
      const propsWithCommonAbilities = {
        ...defaultProps,
        item: { ...defaultProps.item, abilities: commonAbilities }
      };
      
      render(<Editor {...propsWithCommonAbilities} />);
      
      commonAbilities.forEach((ability, index) => {
        expect(screen.getByTestId(`ability-editor-${index}`)).toBeInTheDocument();
      });
    });

    it('handles racial abilities with positive and negative costs', () => {
      const mixedAbilities = [
        { name: 'Powerful', description: 'Strong racial trait', cost: 3 },
        { name: 'Weakness', description: 'Racial limitation', cost: -2 },
        { name: 'Neutral', description: 'No cost trait', cost: 0 }
      ];
      
      const propsWithMixedAbilities = {
        ...defaultProps,
        item: { ...defaultProps.item, abilities: mixedAbilities }
      };
      
      render(<Editor {...propsWithMixedAbilities} />);
      
      expect(screen.getByDisplayValue('3')).toBeInTheDocument();
      expect(screen.getByDisplayValue('-2')).toBeInTheDocument();
      expect(screen.getByDisplayValue('0')).toBeInTheDocument();
    });

    it('handles fantasy setting races', () => {
      const fantasyRaces = [
        { name: 'Dragonborn', description: 'Draconic heritage' },
        { name: 'Tiefling', description: 'Fiendish bloodline' },
        { name: 'Gnome', description: 'Small but clever' }
      ];
      
      fantasyRaces.forEach(race => {
        const props = {
          ...defaultProps,
          item: { ...race, abilities: [] }
        };
        
        render(<Editor {...props} />);
        expect(screen.getByDisplayValue(race.name)).toBeInTheDocument();
      });
    });

    it('handles sci-fi setting races', () => {
      const sciFiRaces = [
        { name: 'Android', description: 'Artificial beings' },
        { name: 'Rakashan', description: 'Feline humanoids' },
        { name: 'Saurian', description: 'Reptilian species' }
      ];
      
      sciFiRaces.forEach(race => {
        const props = {
          ...defaultProps,
          item: { ...race, abilities: [] }
        };
        
        render(<Editor {...props} />);
        expect(screen.getByDisplayValue(race.name)).toBeInTheDocument();
      });
    });
  });

  describe('Accessibility', () => {
    it('has required fields marked as required', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-itemFormName_0');
      const descTextarea = screen.getByTestId('textarea-itemFormDescription_0');
      
      expect(nameInput).toHaveAttribute('required');
      expect(descTextarea).toHaveAttribute('required');
    });

    it('has proper field types', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-itemFormName_0');
      expect(nameInput).toHaveAttribute('type', 'text');
    });

    it('has proper labeling for form fields', () => {
      render(<Editor {...defaultProps} />);
      
      expect(screen.getByText('Race')).toBeInTheDocument();
      expect(screen.getByText('Description')).toBeInTheDocument();
      expect(screen.getByText('Racial Abilities')).toBeInTheDocument();
    });

    it('has accessible button for adding abilities', () => {
      render(<Editor {...defaultProps} />);
      
      const addButton = screen.getByRole('button', { name: 'Add' });
      expect(addButton).toHaveClass('btn');
      expect(addButton).toHaveClass('btn-default');
    });
  });

  describe('Edge Cases', () => {
    it('handles undefined item properties gracefully', () => {
      const propsWithUndefined = {
        ...defaultProps,
        item: {
          name: '',
          description: '',
          abilities: []
        }
      };
      
      expect(() => render(<Editor {...propsWithUndefined} />)).not.toThrow();
    });

    it('handles null item properties gracefully', () => {
      const propsWithNulls = {
        ...defaultProps,
        item: {
          name: '',
          description: '',
          abilities: []
        }
      };
      
      expect(() => render(<Editor {...propsWithNulls} />)).not.toThrow();
    });

    it('handles very long text inputs', () => {
      const longText = 'A'.repeat(1000);
      const props = {
        ...defaultProps,
        item: { 
          name: longText,
          description: 'Different description',
          abilities: []
        }
      };
      
      render(<Editor {...props} />);
      expect(screen.getByDisplayValue(longText)).toBeInTheDocument();
      expect(screen.getByDisplayValue('Different description')).toBeInTheDocument();
    });

    it('handles special characters in text inputs', () => {
      const specialText = 'Race with "quotes" & symbols <>!@#$%^&*()';
      const props = {
        ...defaultProps,
        item: { 
          name: specialText,
          description: 'Different special description',
          abilities: []
        }
      };
      
      render(<Editor {...props} />);
      expect(screen.getByDisplayValue(specialText)).toBeInTheDocument();
      expect(screen.getByDisplayValue('Different special description')).toBeInTheDocument();
    });

    it('handles empty string values', () => {
      const propsWithEmptyStrings = {
        ...defaultProps,
        item: {
          name: '',
          description: '',
          abilities: []
        }
      };
      
      render(<Editor {...propsWithEmptyStrings} />);
      
      const nameInput = screen.getByTestId('text-input-itemFormName_0');
      const descTextarea = screen.getByTestId('textarea-itemFormDescription_0');
      
      expect(nameInput.value).toBe('');
      expect(descTextarea.value).toBe('');
    });

    it('handles large numbers of abilities', () => {
      const manyAbilities = Array.from({ length: 10 }, (_, i) => ({
        name: `Ability ${i + 1}`,
        description: `Description ${i + 1}`,
        cost: i + 1
      }));
      
      const propsWithManyAbilities = {
        ...defaultProps,
        item: { ...defaultProps.item, abilities: manyAbilities }
      };
      
      render(<Editor {...propsWithManyAbilities} />);
      
      manyAbilities.forEach((ability, index) => {
        expect(screen.getByTestId(`ability-editor-${index}`)).toBeInTheDocument();
      });
    });

    it('handles missing abilities array gracefully', () => {
      const propsWithoutAbilities = {
        ...defaultProps,
        item: {
          name: 'Test Race',
          description: 'Test Description',
          abilities: []
        }
      };
      
      expect(() => render(<Editor {...propsWithoutAbilities} />)).not.toThrow();
    });
  });

  describe('Component Integration', () => {
    it('passes correct props to BaseEditor', () => {
      render(<Editor {...defaultProps} />);
      
      const baseEditor = screen.getByTestId('base-editor-test-race-editor');
      expect(baseEditor).toBeInTheDocument();
      
      const deleteButton = screen.getByTestId('delete-button-test-race-editor');
      expect(deleteButton).toBeInTheDocument();
    });

    it('passes correct props to AbilityEditor components', () => {
      render(<Editor {...defaultProps} />);
      
      // Check that abilities are rendered with correct indices
      expect(screen.getByTestId('ability-editor-0')).toBeInTheDocument();
      expect(screen.getByTestId('ability-editor-1')).toBeInTheDocument();
      
      // Check that ability data is displayed
      expect(screen.getByDisplayValue('Extra Edge')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Adaptable')).toBeInTheDocument();
    });

    it('maintains component state consistency across updates', () => {
      const { rerender } = render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-itemFormName_0');
      fireEvent.change(nameInput, { target: { value: 'Updated Race' } });
      
      const updatedProps = {
        ...defaultProps,
        item: { ...defaultProps.item, name: 'Updated Race' }
      };
      
      rerender(<Editor {...updatedProps} />);
      expect(screen.getByDisplayValue('Updated Race')).toBeInTheDocument();
    });

    it('handles multiple Editor instances with different IDs', () => {
      const props1 = { ...defaultProps, id: 'race-1', index: 0 };
      const props2 = { ...defaultProps, id: 'race-2', index: 1 };
      
      const { container } = render(
        <div>
          <Editor {...props1} />
          <Editor {...props2} />
        </div>
      );
      
      expect(container.querySelectorAll('[data-testid^="base-editor-"]')).toHaveLength(2);
    });
  });

  describe('PropTypes Validation', () => {
    it('renders correctly with all required props', () => {
      const validProps = {
        id: 'valid-editor',
        index: 0,
        item: {
          name: 'Valid Race',
          description: 'Valid Description',
          abilities: [
            { name: 'Valid Ability', description: 'Valid Ability Description', cost: 1 }
          ]
        },
        onChange: jest.fn(),
        onDelete: jest.fn()
      };
      
      expect(() => render(<Editor {...validProps} />)).not.toThrow();
    });

    it('handles optional error props correctly', () => {
      const propsWithOptionalErrors = {
        ...defaultProps,
        nameError: 'Optional name error',
        descriptionError: 'Optional description error'
      };
      
      render(<Editor {...propsWithOptionalErrors} />);
      
      expect(screen.getByTestId('error-itemFormName_0')).toBeInTheDocument();
      expect(screen.getByTestId('error-itemFormDescription_0')).toBeInTheDocument();
    });

    it('handles missing optional props gracefully', () => {
      const minimalProps = {
        id: 'minimal-editor',
        index: 0,
        item: {
          name: 'Minimal Race',
          description: 'Minimal Description',
          abilities: []
        },
        onChange: jest.fn(),
        onDelete: jest.fn()
      };
      
      expect(() => render(<Editor {...minimalProps} />)).not.toThrow();
    });
  });

  describe('Complex Interaction Scenarios', () => {
    it('handles adding multiple abilities in sequence', () => {
      render(<Editor {...defaultProps} />);
      
      const addButton = screen.getByRole('button', { name: 'Add' });
      
      // Add first ability
      fireEvent.click(addButton);
      expect(defaultProps.onChange).toHaveBeenCalledTimes(1);
      
      // Reset mock for next test
      defaultProps.onChange.mockClear();
      
      // Add second ability
      fireEvent.click(addButton);
      expect(defaultProps.onChange).toHaveBeenCalledTimes(1);
    });

    it('handles modifying multiple abilities', () => {
      render(<Editor {...defaultProps} />);
      
      // Modify first ability
      const firstAbilityName = screen.getByTestId('ability-name-0');
      fireEvent.change(firstAbilityName, { target: { value: 'Modified First' } });
      
      expect(defaultProps.onChange).toHaveBeenLastCalledWith(
        expect.objectContaining({
          abilities: expect.arrayContaining([
            expect.objectContaining({ name: 'Modified First' })
          ])
        }),
        0
      );
      
      // Modify second ability
      const secondAbilityCost = screen.getByTestId('ability-cost-1');
      fireEvent.change(secondAbilityCost, { target: { value: '5' } });
      
      expect(defaultProps.onChange).toHaveBeenLastCalledWith(
        expect.objectContaining({
          abilities: expect.arrayContaining([
            expect.objectContaining({ cost: 5 })
          ])
        }),
        0
      );
    });

    it('handles race field changes while managing abilities', () => {
      render(<Editor {...defaultProps} />);
      
      // Change race name
      const nameInput = screen.getByTestId('text-input-itemFormName_0');
      fireEvent.change(nameInput, { target: { value: 'Modified Race' } });
      
      // Add an ability
      const addButton = screen.getByRole('button', { name: 'Add' });
      fireEvent.click(addButton);
      
      // Verify both operations maintain data integrity
      const lastCall = defaultProps.onChange.mock.calls[defaultProps.onChange.mock.calls.length - 1];
      expect(lastCall[0].name).toBe('Human'); // Should maintain original name from addRacialAbility
      expect(lastCall[0].abilities).toHaveLength(3); // Original 2 + 1 new
    });
  });
});
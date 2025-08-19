import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import Editor from './Editor';

// Mock the dependencies
jest.mock('bootstrap-react-components', () => ({
  NumberFormGroup: ({ id, label, onChange, value, required }) => (
    <div data-testid={`number-form-${id}`}>
      <label>{label}</label>
      <input
        type="number"
        onChange={onChange}
        value={value || ''}
        required={required}
        data-testid={`number-input-${id}`}
      />
    </div>
  ),
  TextAreaFormGroup: ({ id, label, onChange, value, required }) => (
    <div data-testid={`textarea-form-${id}`}>
      <label>{label}</label>
      <textarea
        onChange={onChange}
        value={value || ''}
        required={required}
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

jest.mock('../../../formgroups/AttributeSelectFormGroup', () => {
  return function MockAttributeSelectFormGroup({ id, onChange, attribute }) {
    const attributes = ['Agility', 'Smarts', 'Spirit', 'Strength', 'Vigor'];
    return (
      <div data-testid={`attribute-select-${id}`}>
        <label>Attribute</label>
        <select
          onChange={onChange}
          value={attribute || ''}
          data-testid={`attribute-select-input-${id}`}
        >
          <option value="">Select Attribute</option>
          {attributes.map(attr => (
            <option key={attr} value={attr}>{attr}</option>
          ))}
        </select>
      </div>
    );
  };
});

describe('Arcane Backgrounds Editor Component', () => {
  const defaultProps = {
    id: 'test-arcane-background',
    index: 0,
    item: {
      name: 'Magic',
      description: 'The ancient art of spellcasting',
      skillName: 'Spellcasting',
      attribute: 'Smarts',
      startingPowers: 3,
      startingPowerPoints: 10
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
      expect(screen.getByTestId('base-editor-test-arcane-background')).toBeInTheDocument();
    });

    it('renders all form fields with correct labels', () => {
      render(<Editor {...defaultProps} />);
      
      expect(screen.getByText('Name')).toBeInTheDocument();
      expect(screen.getByText('Description')).toBeInTheDocument();
      expect(screen.getByText('Skill')).toBeInTheDocument();
      expect(screen.getByText('Attribute')).toBeInTheDocument();
      expect(screen.getByText('Starting Powers')).toBeInTheDocument();
      expect(screen.getByText('Starting Power Points')).toBeInTheDocument();
    });

    it('displays all field values correctly', () => {
      render(<Editor {...defaultProps} />);
      
      expect(screen.getByDisplayValue('Magic')).toBeInTheDocument();
      expect(screen.getByDisplayValue('The ancient art of spellcasting')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Spellcasting')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Smarts')).toBeInTheDocument();
      expect(screen.getByDisplayValue('3')).toBeInTheDocument();
      expect(screen.getByDisplayValue('10')).toBeInTheDocument();
    });

    it('generates correct component IDs', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-ArcaneBackgroundEditor-0-test-arcane-background-Name');
      const descTextarea = screen.getByTestId('textarea-ArcaneBackgroundEditor-0-test-arcane-background-Description');
      const skillInput = screen.getByTestId('text-input-ArcaneBackgroundEditor-0-test-arcane-background-Skill');
      const attributeSelect = screen.getByTestId('attribute-select-input-ArcaneBackgroundEditor-0-test-arcane-background-SkillAttribute');
      const powersInput = screen.getByTestId('number-input-ArcaneBackgroundEditor-0-test-arcane-background-StartingPowers');
      const pointsInput = screen.getByTestId('number-input-ArcaneBackgroundEditor-0-test-arcane-background-StartPowerPoints');
      
      expect(nameInput).toBeInTheDocument();
      expect(descTextarea).toBeInTheDocument();
      expect(skillInput).toBeInTheDocument();
      expect(attributeSelect).toBeInTheDocument();
      expect(powersInput).toBeInTheDocument();
      expect(pointsInput).toBeInTheDocument();
    });
  });

  describe('Form Field Updates', () => {
    it('updates name field correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-ArcaneBackgroundEditor-0-test-arcane-background-Name');
      fireEvent.change(nameInput, { target: { value: 'Miracles' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, name: 'Miracles' },
        0
      );
    });

    it('updates description field correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const descTextarea = screen.getByTestId('textarea-ArcaneBackgroundEditor-0-test-arcane-background-Description');
      fireEvent.change(descTextarea, { target: { value: 'Divine power channeled through faith' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, description: 'Divine power channeled through faith' },
        0
      );
    });

    it('updates skill name field correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const skillInput = screen.getByTestId('text-input-ArcaneBackgroundEditor-0-test-arcane-background-Skill');
      fireEvent.change(skillInput, { target: { value: 'Faith' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, skillName: 'Faith' },
        0
      );
    });

    it('updates attribute field correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const attributeSelect = screen.getByTestId('attribute-select-input-ArcaneBackgroundEditor-0-test-arcane-background-SkillAttribute');
      fireEvent.change(attributeSelect, { target: { value: 'Spirit' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, attribute: 'Spirit' },
        0
      );
    });

    it('updates starting powers field correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const powersInput = screen.getByTestId('number-input-ArcaneBackgroundEditor-0-test-arcane-background-StartingPowers');
      fireEvent.change(powersInput, { target: { value: '2' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, startingPowers: 2 },
        0
      );
    });

    it('updates starting power points field correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const pointsInput = screen.getByTestId('number-input-ArcaneBackgroundEditor-0-test-arcane-background-StartPowerPoints');
      fireEvent.change(pointsInput, { target: { value: '15' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, startingPowerPoints: 15 },
        0
      );
    });
  });

  describe('Number Field Validation', () => {
    it('handles zero starting powers', () => {
      render(<Editor {...defaultProps} />);
      
      const powersInput = screen.getByTestId('number-input-ArcaneBackgroundEditor-0-test-arcane-background-StartingPowers');
      fireEvent.change(powersInput, { target: { value: '0' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, startingPowers: 0 },
        0
      );
    });

    it('handles zero starting power points', () => {
      render(<Editor {...defaultProps} />);
      
      const pointsInput = screen.getByTestId('number-input-ArcaneBackgroundEditor-0-test-arcane-background-StartPowerPoints');
      fireEvent.change(pointsInput, { target: { value: '0' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, startingPowerPoints: 0 },
        0
      );
    });

    it('handles empty string for starting powers as 0', () => {
      render(<Editor {...defaultProps} />);
      
      const powersInput = screen.getByTestId('number-input-ArcaneBackgroundEditor-0-test-arcane-background-StartingPowers');
      fireEvent.change(powersInput, { target: { value: '' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, startingPowers: 0 },
        0
      );
    });

    it('handles non-numeric input for starting powers as 0', () => {
      render(<Editor {...defaultProps} />);
      
      const powersInput = screen.getByTestId('number-input-ArcaneBackgroundEditor-0-test-arcane-background-StartingPowers');
      fireEvent.change(powersInput, { target: { value: 'abc' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, startingPowers: 0 },
        0
      );
    });

    it('handles floating point numbers by converting to integer', () => {
      render(<Editor {...defaultProps} />);
      
      const powersInput = screen.getByTestId('number-input-ArcaneBackgroundEditor-0-test-arcane-background-StartingPowers');
      fireEvent.change(powersInput, { target: { value: '3.7' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, startingPowers: 3 },
        0
      );
    });
  });

  describe('Delete Functionality', () => {
    it('calls onDelete with correct index when delete button is clicked', () => {
      render(<Editor {...defaultProps} />);
      
      const deleteButton = screen.getByTestId('delete-button-test-arcane-background');
      fireEvent.click(deleteButton);
      
      expect(defaultProps.onDelete).toHaveBeenCalledWith(0);
    });

    it('prevents default event when delete is triggered', () => {
      const mockEvent = { preventDefault: jest.fn() };
      const component = new Editor(defaultProps);
      
      component.onDelete(mockEvent);
      
      expect(mockEvent.preventDefault).toHaveBeenCalled();
      expect(defaultProps.onDelete).toHaveBeenCalledWith(0);
    });
  });

  describe('Savage Worlds Arcane Backgrounds', () => {
    const savageWorldsBackgrounds = [
      {
        name: 'Magic',
        description: 'Wizards, sorcerers, and other practitioners of the arcane arts',
        skillName: 'Spellcasting',
        attribute: 'Smarts',
        startingPowers: 3,
        startingPowerPoints: 10
      },
      {
        name: 'Miracles',
        description: 'Clerics, paladins, and other holy warriors',
        skillName: 'Faith',
        attribute: 'Spirit',
        startingPowers: 3,
        startingPowerPoints: 10
      },
      {
        name: 'Psionics',
        description: 'Mental powers and telepathic abilities',
        skillName: 'Psionics',
        attribute: 'Smarts',
        startingPowers: 3,
        startingPowerPoints: 10
      },
      {
        name: 'Weird Science',
        description: 'Mad scientists and their bizarre inventions',
        skillName: 'Weird Science',
        attribute: 'Smarts',
        startingPowers: 2,
        startingPowerPoints: 15
      },
      {
        name: 'Chi',
        description: 'Martial artists who channel inner energy',
        skillName: 'Chi',
        attribute: 'Spirit',
        startingPowers: 1,
        startingPowerPoints: 10
      }
    ];

    it('handles all standard Savage Worlds arcane backgrounds', () => {
      savageWorldsBackgrounds.forEach((background, index) => {
        const props = { ...defaultProps, item: background, id: `background-${index}` };
        const { container } = render(<Editor {...props} />);
        
        expect(container.querySelector('input[type="text"]')).toHaveValue(background.name);
        expect(container.querySelector('textarea')).toHaveValue(background.description);
        expect(container.querySelector('input[value="' + background.skillName + '"]')).toHaveValue(background.skillName);
        expect(container.querySelector('select')).toHaveValue(background.attribute);
      });
    });

    it('handles custom arcane backgrounds', () => {
      const customBackground = {
        name: 'Elementalism',
        description: 'Masters of the four classical elements',
        skillName: 'Elementalism',
        attribute: 'Smarts',
        startingPowers: 2,
        startingPowerPoints: 12
      };
      
      const props = { ...defaultProps, item: customBackground };
      const { container } = render(<Editor {...props} />);
      
      expect(container.querySelector('input[type="text"]')).toHaveValue('Elementalism');
      expect(container.querySelector('textarea')).toHaveValue('Masters of the four classical elements');
    });
  });

  describe('Attribute Selection', () => {
    const attributes = ['Agility', 'Smarts', 'Spirit', 'Strength', 'Vigor'];

    attributes.forEach(attribute => {
      it(`handles ${attribute} attribute selection`, () => {
        const props = { ...defaultProps, item: { ...defaultProps.item, attribute } };
        render(<Editor {...props} />);
        
        const attributeSelect = screen.getByTestId('attribute-select-input-ArcaneBackgroundEditor-0-test-arcane-background-SkillAttribute');
        expect(attributeSelect).toHaveValue(attribute);
      });
    });

    it('handles empty attribute selection', () => {
      const props = { ...defaultProps, item: { ...defaultProps.item, attribute: '' } };
      render(<Editor {...props} />);
      
      const attributeSelect = screen.getByTestId('attribute-select-input-ArcaneBackgroundEditor-0-test-arcane-background-SkillAttribute');
      expect(attributeSelect).toHaveValue('');
    });
  });

  describe('Accessibility', () => {
    it('has required fields marked as required', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-ArcaneBackgroundEditor-0-test-arcane-background-Name');
      const skillInput = screen.getByTestId('text-input-ArcaneBackgroundEditor-0-test-arcane-background-Skill');
      const powersInput = screen.getByTestId('number-input-ArcaneBackgroundEditor-0-test-arcane-background-StartingPowers');
      const pointsInput = screen.getByTestId('number-input-ArcaneBackgroundEditor-0-test-arcane-background-StartPowerPoints');
      
      expect(nameInput).toHaveAttribute('required');
      expect(skillInput).toHaveAttribute('required');
      expect(powersInput).toHaveAttribute('required');
      expect(pointsInput).toHaveAttribute('required');
    });

    it('has proper field types', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-ArcaneBackgroundEditor-0-test-arcane-background-Name');
      const skillInput = screen.getByTestId('text-input-ArcaneBackgroundEditor-0-test-arcane-background-Skill');
      const powersInput = screen.getByTestId('number-input-ArcaneBackgroundEditor-0-test-arcane-background-StartingPowers');
      const pointsInput = screen.getByTestId('number-input-ArcaneBackgroundEditor-0-test-arcane-background-StartPowerPoints');
      
      expect(nameInput).toHaveAttribute('type', 'text');
      expect(skillInput).toHaveAttribute('type', 'text');
      expect(powersInput).toHaveAttribute('type', 'number');
      expect(pointsInput).toHaveAttribute('type', 'number');
    });
  });

  describe('Edge Cases', () => {
    it('handles undefined item properties gracefully', () => {
      const propsWithUndefined = {
        ...defaultProps,
        item: {}
      };
      
      expect(() => render(<Editor {...propsWithUndefined} />)).not.toThrow();
    });

    it('handles null item properties gracefully', () => {
      const propsWithNulls = {
        ...defaultProps,
        item: {
          name: null,
          description: null,
          skillName: null,
          attribute: null,
          startingPowers: null,
          startingPowerPoints: null
        }
      };
      
      expect(() => render(<Editor {...propsWithNulls} />)).not.toThrow();
    });

    it('handles very long text inputs', () => {
      const longText = 'A'.repeat(1000);
      const props = {
        ...defaultProps,
        item: { ...defaultProps.item, description: longText }
      };
      
      render(<Editor {...props} />);
      expect(screen.getByDisplayValue(longText)).toBeInTheDocument();
    });

    it('handles special characters in text inputs', () => {
      const specialText = 'Background with "quotes" & symbols <>!@#$%^&*()';
      const props = {
        ...defaultProps,
        item: { ...defaultProps.item, name: specialText }
      };
      
      render(<Editor {...props} />);
      expect(screen.getByDisplayValue(specialText)).toBeInTheDocument();
    });

    it('handles different index values', () => {
      const props = { ...defaultProps, index: 3 };
      render(<Editor {...props} />);
      
      const nameInput = screen.getByTestId('text-input-ArcaneBackgroundEditor-3-test-arcane-background-Name');
      fireEvent.change(nameInput, { target: { value: 'Test' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, name: 'Test' },
        3
      );
    });
  });

  describe('Component Integration', () => {
    it('passes correct props to BaseEditor', () => {
      render(<Editor {...defaultProps} />);
      
      const baseEditor = screen.getByTestId('base-editor-test-arcane-background');
      expect(baseEditor).toBeInTheDocument();
      
      const deleteButton = screen.getByTestId('delete-button-test-arcane-background');
      expect(deleteButton).toBeInTheDocument();
    });

    it('maintains component state consistency across updates', () => {
      const { rerender } = render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-ArcaneBackgroundEditor-0-test-arcane-background-Name');
      fireEvent.change(nameInput, { target: { value: 'New Background' } });
      
      const updatedProps = {
        ...defaultProps,
        item: { ...defaultProps.item, name: 'New Background' }
      };
      
      rerender(<Editor {...updatedProps} />);
      expect(screen.getByDisplayValue('New Background')).toBeInTheDocument();
    });

    it('handles multiple instances with different IDs', () => {
      const props1 = { ...defaultProps, id: 'background-1', index: 0 };
      const props2 = { ...defaultProps, id: 'background-2', index: 1 };
      
      const { container } = render(
        <div>
          <Editor {...props1} />
          <Editor {...props2} />
        </div>
      );
      
      expect(container.querySelectorAll('[data-testid^="base-editor-"]')).toHaveLength(2);
    });
  });

  describe('Form Validation Scenarios', () => {
    it('handles form submission with empty required fields', () => {
      const emptyProps = {
        ...defaultProps,
        item: {
          name: '',
          description: '',
          skillName: '',
          attribute: '',
          startingPowers: 0,
          startingPowerPoints: 0
        }
      };
      
      render(<Editor {...emptyProps} />);
      
      const nameInput = screen.getByTestId('text-input-ArcaneBackgroundEditor-0-test-arcane-background-Name');
      const skillInput = screen.getByTestId('text-input-ArcaneBackgroundEditor-0-test-arcane-background-Skill');
      
      expect(nameInput).toHaveAttribute('required');
      expect(skillInput).toHaveAttribute('required');
      expect(nameInput.value).toBe('');
      expect(skillInput.value).toBe('');
    });

    it('handles valid form data', () => {
      const validProps = {
        ...defaultProps,
        item: {
          name: 'Valid Background',
          description: 'A valid arcane background',
          skillName: 'Valid Skill',
          attribute: 'Smarts',
          startingPowers: 2,
          startingPowerPoints: 12
        }
      };
      
      render(<Editor {...validProps} />);
      
      expect(screen.getByDisplayValue('Valid Background')).toBeInTheDocument();
      expect(screen.getByDisplayValue('A valid arcane background')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Valid Skill')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Smarts')).toBeInTheDocument();
      expect(screen.getByDisplayValue('2')).toBeInTheDocument();
      expect(screen.getByDisplayValue('12')).toBeInTheDocument();
    });
  });

  describe('Power Point and Power Count Edge Cases', () => {
    it('handles very large starting powers', () => {
      render(<Editor {...defaultProps} />);
      
      const powersInput = screen.getByTestId('number-input-ArcaneBackgroundEditor-0-test-arcane-background-StartingPowers');
      fireEvent.change(powersInput, { target: { value: '99' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, startingPowers: 99 },
        0
      );
    });

    it('handles very large starting power points', () => {
      render(<Editor {...defaultProps} />);
      
      const pointsInput = screen.getByTestId('number-input-ArcaneBackgroundEditor-0-test-arcane-background-StartPowerPoints');
      fireEvent.change(pointsInput, { target: { value: '999' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, startingPowerPoints: 999 },
        0
      );
    });

    it('handles typical Savage Worlds power combinations', () => {
      const typicalCombinations = [
        { powers: 1, points: 10 }, // Low magic settings
        { powers: 2, points: 15 }, // Weird Science
        { powers: 3, points: 10 }, // Standard Magic/Miracles
        { powers: 4, points: 20 }  // High magic settings
      ];
      
      typicalCombinations.forEach(combo => {
        const props = {
          ...defaultProps,
          item: { ...defaultProps.item, startingPowers: combo.powers, startingPowerPoints: combo.points }
        };
        
        const { container } = render(<Editor {...props} />);
        const powersInput = container.querySelector('input[type="number"]');
        expect(powersInput.value).toBe(combo.powers.toString());
      });
    });
  });
});
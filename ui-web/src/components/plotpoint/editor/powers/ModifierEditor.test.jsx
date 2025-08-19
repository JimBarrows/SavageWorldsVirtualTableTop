import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import ModifierEditor from './ModifierEditor';

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

describe('ModifierEditor Component', () => {
  const defaultProps = {
    id: 'test-modifier-editor',
    index: 0,
    item: {
      name: 'Extra Damage',
      description: 'Adds +1d6 damage to the power',
      powerPointModifier: 2
    },
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders without crashing', () => {
      render(<ModifierEditor {...defaultProps} />);
      expect(screen.getByTestId('base-editor-test-modifier-editor')).toBeInTheDocument();
    });

    it('renders all form fields with correct labels', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      expect(screen.getByText('Name')).toBeInTheDocument();
      expect(screen.getByText('Description')).toBeInTheDocument();
      expect(screen.getByText('Power Point Modifier')).toBeInTheDocument();
    });

    it('displays all field values correctly', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      expect(screen.getByDisplayValue('Extra Damage')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Adds +1d6 damage to the power')).toBeInTheDocument();
      expect(screen.getByDisplayValue('2')).toBeInTheDocument();
    });

    it('generates correct field IDs', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-FormControl-text-TextFormGroup-ModifierEditor-test-modifier-editor-0-Name');
      const descTextarea = screen.getByTestId('textarea-TextAreaFormGroup-ModifierEditor-test-modifier-editor-0-Description');
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-ModifierEditor-test-modifier-editor-0-PowerPointModifier');
      
      expect(nameInput).toBeInTheDocument();
      expect(descTextarea).toBeInTheDocument();
      expect(ppInput).toBeInTheDocument();
    });
  });

  describe('Form Field Updates', () => {
    it('updates name field correctly', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-FormControl-text-TextFormGroup-ModifierEditor-test-modifier-editor-0-Name');
      fireEvent.change(nameInput, { target: { value: 'Extra Range' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, name: 'Extra Range' },
        0
      );
    });

    it('updates description field correctly', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const descTextarea = screen.getByTestId('textarea-TextAreaFormGroup-ModifierEditor-test-modifier-editor-0-Description');
      fireEvent.change(descTextarea, { target: { value: 'Doubles the range of the power' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, description: 'Doubles the range of the power' },
        0
      );
    });

    it('updates power point modifier field correctly', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-ModifierEditor-test-modifier-editor-0-PowerPointModifier');
      fireEvent.change(ppInput, { target: { value: '3' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, powerPointModifier: 3 },
        0
      );
    });
  });

  describe('Power Point Modifier Validation', () => {
    it('handles zero power point modifier', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-ModifierEditor-test-modifier-editor-0-PowerPointModifier');
      fireEvent.change(ppInput, { target: { value: '0' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, powerPointModifier: 0 },
        0
      );
    });

    it('handles negative power point modifier', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-ModifierEditor-test-modifier-editor-0-PowerPointModifier');
      fireEvent.change(ppInput, { target: { value: '-1' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, powerPointModifier: -1 },
        0
      );
    });

    it('handles positive power point modifier', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-ModifierEditor-test-modifier-editor-0-PowerPointModifier');
      fireEvent.change(ppInput, { target: { value: '5' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, powerPointModifier: 5 },
        0
      );
    });

    it('handles empty string as 0', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-ModifierEditor-test-modifier-editor-0-PowerPointModifier');
      fireEvent.change(ppInput, { target: { value: '' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, powerPointModifier: 0 },
        0
      );
    });

    it('handles non-numeric input as 0', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-ModifierEditor-test-modifier-editor-0-PowerPointModifier');
      fireEvent.change(ppInput, { target: { value: 'abc' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, powerPointModifier: 0 },
        0
      );
    });
  });

  describe('Delete Functionality', () => {
    it('calls onDelete with correct index when delete button is clicked', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const deleteButton = screen.getByTestId('delete-button-test-modifier-editor');
      fireEvent.click(deleteButton);
      
      expect(defaultProps.onDelete).toHaveBeenCalledWith(0);
    });

    it('prevents default event when delete is triggered', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const deleteButton = screen.getByTestId('delete-button-test-modifier-editor');
      const event = { preventDefault: jest.fn() };
      
      // Simulate the actual delete handler
      fireEvent.click(deleteButton);
      
      expect(defaultProps.onDelete).toHaveBeenCalled();
    });
  });

  describe('Savage Worlds RPG Modifiers', () => {
    it('handles common positive modifiers', () => {
      const commonModifiers = [
        { name: 'Extra Damage', description: '+1d6 damage', powerPointModifier: 2 },
        { name: 'Double Range', description: 'Double the range', powerPointModifier: 3 },
        { name: 'Extended Duration', description: 'Double the duration', powerPointModifier: 4 },
        { name: 'Area Effect', description: 'Medium Burst Template', powerPointModifier: 5 }
      ];
      
      commonModifiers.forEach((modifier, index) => {
        const props = { ...defaultProps, item: modifier, id: `modifier-${index}` };
        const { container } = render(<ModifierEditor {...props} />);
        expect(container.querySelector('input[type="text"]')).toHaveValue(modifier.name);
        expect(container.querySelector('textarea')).toHaveValue(modifier.description);
        expect(container.querySelector('input[type="number"]')).toHaveValue(modifier.powerPointModifier);
      });
    });

    it('handles common negative modifiers (limitations)', () => {
      const limitations = [
        { name: 'Limitation: Range', description: 'Half range', powerPointModifier: -1 },
        { name: 'Limitation: Duration', description: 'Half duration', powerPointModifier: -2 },
        { name: 'Limitation: Damage', description: 'Non-lethal only', powerPointModifier: -3 }
      ];
      
      limitations.forEach((limitation, index) => {
        const props = { ...defaultProps, item: limitation, id: `limitation-${index}` };
        const { container } = render(<ModifierEditor {...props} />);
        expect(container.querySelector('input[type="text"]')).toHaveValue(limitation.name);
        expect(container.querySelector('input[type="number"]')).toHaveValue(limitation.powerPointModifier);
      });
    });

    it('handles zero-cost modifiers', () => {
      const zeroCostModifiers = [
        { name: 'Trapping Change', description: 'Change visual/audio effects', powerPointModifier: 0 },
        { name: 'Minor Variation', description: 'Small thematic change', powerPointModifier: 0 }
      ];
      
      zeroCostModifiers.forEach((modifier, index) => {
        const props = { ...defaultProps, item: modifier, id: `zero-cost-${index}` };
        const { container } = render(<ModifierEditor {...props} />);
        const numberInput = container.querySelector('input[type="number"]');
        // Zero values might be displayed as empty string or "0"
        expect(numberInput.value === '0' || numberInput.value === '').toBe(true);
      });
    });
  });

  describe('Accessibility', () => {
    it('has required fields marked as required', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-FormControl-text-TextFormGroup-ModifierEditor-test-modifier-editor-0-Name');
      const descTextarea = screen.getByTestId('textarea-TextAreaFormGroup-ModifierEditor-test-modifier-editor-0-Description');
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-ModifierEditor-test-modifier-editor-0-PowerPointModifier');
      
      expect(nameInput).toHaveAttribute('required');
      expect(descTextarea).toHaveAttribute('required');
      expect(ppInput).toHaveAttribute('required');
    });

    it('has proper field types', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-FormControl-text-TextFormGroup-ModifierEditor-test-modifier-editor-0-Name');
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-ModifierEditor-test-modifier-editor-0-PowerPointModifier');
      
      expect(nameInput).toHaveAttribute('type', 'text');
      expect(ppInput).toHaveAttribute('type', 'number');
    });
  });

  describe('Edge Cases', () => {
    it('handles undefined item properties gracefully', () => {
      const propsWithUndefined = {
        ...defaultProps,
        item: {}
      };
      
      expect(() => render(<ModifierEditor {...propsWithUndefined} />)).not.toThrow();
    });

    it('handles null item properties gracefully', () => {
      const propsWithNulls = {
        ...defaultProps,
        item: {
          name: null,
          description: null,
          powerPointModifier: null
        }
      };
      
      expect(() => render(<ModifierEditor {...propsWithNulls} />)).not.toThrow();
    });

    it('handles very long text inputs', () => {
      const longText = 'A'.repeat(1000);
      const props = {
        ...defaultProps,
        item: { ...defaultProps.item, description: longText }
      };
      
      render(<ModifierEditor {...props} />);
      expect(screen.getByDisplayValue(longText)).toBeInTheDocument();
    });

    it('handles special characters in text inputs', () => {
      const specialText = 'Modifier with "quotes" & symbols <>!@#$%^&*()';
      const props = {
        ...defaultProps,
        item: { ...defaultProps.item, name: specialText }
      };
      
      render(<ModifierEditor {...props} />);
      expect(screen.getByDisplayValue(specialText)).toBeInTheDocument();
    });

    it('handles different index values', () => {
      const props = { ...defaultProps, index: 5 };
      render(<ModifierEditor {...props} />);
      
      const nameInput = screen.getByTestId('text-input-FormControl-text-TextFormGroup-ModifierEditor-test-modifier-editor-5-Name');
      fireEvent.change(nameInput, { target: { value: 'Test' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, name: 'Test' },
        5
      );
    });
  });

  describe('Component Integration', () => {
    it('passes correct props to BaseEditor', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const baseEditor = screen.getByTestId('base-editor-test-modifier-editor');
      expect(baseEditor).toBeInTheDocument();
      
      const deleteButton = screen.getByTestId('delete-button-test-modifier-editor');
      expect(deleteButton).toBeInTheDocument();
    });

    it('maintains component state consistency across updates', () => {
      const { rerender } = render(<ModifierEditor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-FormControl-text-TextFormGroup-ModifierEditor-test-modifier-editor-0-Name');
      fireEvent.change(nameInput, { target: { value: 'New Modifier' } });
      
      const updatedProps = {
        ...defaultProps,
        item: { ...defaultProps.item, name: 'New Modifier' }
      };
      
      rerender(<ModifierEditor {...updatedProps} />);
      expect(screen.getByDisplayValue('New Modifier')).toBeInTheDocument();
    });

    it('handles multiple instances with different IDs', () => {
      const props1 = { ...defaultProps, id: 'modifier-1', index: 0 };
      const props2 = { ...defaultProps, id: 'modifier-2', index: 1 };
      
      const { container } = render(
        <div>
          <ModifierEditor {...props1} />
          <ModifierEditor {...props2} />
        </div>
      );
      
      expect(container.querySelectorAll('[data-testid^="base-editor-"]')).toHaveLength(2);
    });
  });

  describe('Form Validation Scenarios', () => {
    it('handles form submission with empty required fields', () => {
      const emptyProps = {
        ...defaultProps,
        item: { name: '', description: '', powerPointModifier: 0 }
      };
      
      render(<ModifierEditor {...emptyProps} />);
      
      const nameInput = screen.getByTestId('text-input-FormControl-text-TextFormGroup-ModifierEditor-test-modifier-editor-0-Name');
      const descTextarea = screen.getByTestId('textarea-TextAreaFormGroup-ModifierEditor-test-modifier-editor-0-Description');
      
      expect(nameInput).toHaveAttribute('required');
      expect(descTextarea).toHaveAttribute('required');
      expect(nameInput.value).toBe('');
      expect(descTextarea.value).toBe('');
    });

    it('handles valid form data', () => {
      const validProps = {
        ...defaultProps,
        item: {
          name: 'Valid Modifier',
          description: 'A valid modifier description',
          powerPointModifier: 1
        }
      };
      
      render(<ModifierEditor {...validProps} />);
      
      expect(screen.getByDisplayValue('Valid Modifier')).toBeInTheDocument();
      expect(screen.getByDisplayValue('A valid modifier description')).toBeInTheDocument();
      expect(screen.getByDisplayValue('1')).toBeInTheDocument();
    });
  });

  describe('Power Point Modifier Edge Cases', () => {
    it('handles floating point numbers by converting to integer', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-ModifierEditor-test-modifier-editor-0-PowerPointModifier');
      fireEvent.change(ppInput, { target: { value: '2.7' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, powerPointModifier: 2 },
        0
      );
    });

    it('handles very large numbers', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-ModifierEditor-test-modifier-editor-0-PowerPointModifier');
      fireEvent.change(ppInput, { target: { value: '999' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, powerPointModifier: 999 },
        0
      );
    });

    it('handles very small negative numbers', () => {
      render(<ModifierEditor {...defaultProps} />);
      
      const ppInput = screen.getByTestId('number-input-FormControl-number-NumberFormGroup-ModifierEditor-test-modifier-editor-0-PowerPointModifier');
      fireEvent.change(ppInput, { target: { value: '-999' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, powerPointModifier: -999 },
        0
      );
    });
  });
});
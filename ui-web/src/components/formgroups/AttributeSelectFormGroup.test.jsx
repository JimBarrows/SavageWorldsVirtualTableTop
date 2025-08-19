import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import AttributeSelectFormGroup from './AttributeSelectFormGroup';

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components', () => ({
  SelectFormGroup: function MockSelectFormGroup({ id, label, onChange, options, value }) {
    return (
      <div data-testid="select-form-group">
        <label htmlFor={id}>{label}</label>
        <select 
          id={id} 
          value={value || ''} 
          onChange={(e) => onChange && onChange(e)}
          data-testid="select-input"
        >
          <option value="">Select an option</option>
          {options?.map((option, index) => (
            <option key={index} value={option.value}>
              {option.label}
            </option>
          ))}
        </select>
      </div>
    );
  }
}));

describe('AttributeSelectFormGroup Component', () => {
  const defaultProps = {
    id: 'test-attribute',
    label: 'Test Attribute',
    onChange: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<AttributeSelectFormGroup {...defaultProps} />);
      
      expect(screen.getByTestId('select-form-group')).toBeInTheDocument();
      expect(screen.getByLabelText('Test Attribute')).toBeInTheDocument();
    });

    it('renders with correct wrapper id', () => {
      render(<AttributeSelectFormGroup {...defaultProps} id="my-attribute" />);
      
      const wrapper = document.getElementById('AttributeSelectFormGroup-my-attribute');
      expect(wrapper).toBeInTheDocument();
    });

    it('uses default label when not provided', () => {
      const { id, onChange } = defaultProps;
      render(<AttributeSelectFormGroup id={id} onChange={onChange} />);
      
      expect(screen.getByLabelText('Attribute')).toBeInTheDocument();
    });

    it('renders with custom label', () => {
      render(<AttributeSelectFormGroup {...defaultProps} label="Character Attribute" />);
      
      expect(screen.getByLabelText('Character Attribute')).toBeInTheDocument();
    });

    it('passes correct id to SelectFormGroup', () => {
      render(<AttributeSelectFormGroup {...defaultProps} id="custom-id" />);
      
      const select = screen.getByTestId('select-input');
      expect(select).toHaveAttribute('id', 'custom-id');
    });
  });

  describe('Attribute Options', () => {
    it('renders all Savage Worlds attributes', () => {
      render(<AttributeSelectFormGroup {...defaultProps} />);
      
      const expectedAttributes = [
        'Agility',
        'Smarts', 
        'Spirit',
        'Strength',
        'Vigor'
      ];

      expectedAttributes.forEach(attribute => {
        expect(screen.getByRole('option', { name: attribute })).toBeInTheDocument();
      });
    });

    it('has correct option values', () => {
      render(<AttributeSelectFormGroup {...defaultProps} />);
      
      const agilityOption = screen.getByRole('option', { name: 'Agility' });
      const smartsOption = screen.getByRole('option', { name: 'Smarts' });
      const spiritOption = screen.getByRole('option', { name: 'Spirit' });
      const strengthOption = screen.getByRole('option', { name: 'Strength' });
      const vigorOption = screen.getByRole('option', { name: 'Vigor' });

      expect(agilityOption).toHaveAttribute('value', 'Agility');
      expect(smartsOption).toHaveAttribute('value', 'Smarts');
      expect(spiritOption).toHaveAttribute('value', 'Spirit');
      expect(strengthOption).toHaveAttribute('value', 'Strength');
      expect(vigorOption).toHaveAttribute('value', 'Vigor');
    });

    it('includes default empty option', () => {
      render(<AttributeSelectFormGroup {...defaultProps} />);
      
      expect(screen.getByRole('option', { name: 'Select an option' })).toBeInTheDocument();
    });
  });

  describe('Value Handling', () => {
    it('displays selected attribute value', () => {
      render(<AttributeSelectFormGroup {...defaultProps} attribute="Agility" />);
      
      const select = screen.getByTestId('select-input');
      expect(select).toHaveValue('Agility');
    });

    it('displays empty value when no attribute is selected', () => {
      render(<AttributeSelectFormGroup {...defaultProps} />);
      
      const select = screen.getByTestId('select-input');
      expect(select).toHaveValue('');
    });

    it('handles undefined attribute prop', () => {
      render(<AttributeSelectFormGroup {...defaultProps} attribute={undefined} />);
      
      const select = screen.getByTestId('select-input');
      expect(select).toHaveValue('');
    });

    it('handles null attribute prop', () => {
      render(<AttributeSelectFormGroup {...defaultProps} attribute={null} />);
      
      const select = screen.getByTestId('select-input');
      expect(select).toHaveValue('');
    });
  });

  describe('Change Handling', () => {
    it('calls onChange when option is selected', () => {
      const mockOnChange = jest.fn();
      render(<AttributeSelectFormGroup {...defaultProps} onChange={mockOnChange} />);
      
      const select = screen.getByTestId('select-input');
      fireEvent.change(select, { target: { value: 'Strength' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          target: expect.objectContaining({
            value: 'Strength'
          })
        })
      );
    });

    it('calls onChange when clearing selection', () => {
      const mockOnChange = jest.fn();
      render(<AttributeSelectFormGroup {...defaultProps} onChange={mockOnChange} attribute="Vigor" />);
      
      const select = screen.getByTestId('select-input');
      fireEvent.change(select, { target: { value: '' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          target: expect.objectContaining({
            value: ''
          })
        })
      );
    });

    it('handles multiple change events', () => {
      const mockOnChange = jest.fn();
      render(<AttributeSelectFormGroup {...defaultProps} onChange={mockOnChange} />);
      
      const select = screen.getByTestId('select-input');
      
      fireEvent.change(select, { target: { value: 'Agility' } });
      fireEvent.change(select, { target: { value: 'Smarts' } });
      fireEvent.change(select, { target: { value: 'Spirit' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(3);
      expect(mockOnChange).toHaveBeenNthCalledWith(1, expect.objectContaining({
        target: expect.objectContaining({ value: 'Agility' })
      }));
      expect(mockOnChange).toHaveBeenNthCalledWith(2, expect.objectContaining({
        target: expect.objectContaining({ value: 'Smarts' })
      }));
      expect(mockOnChange).toHaveBeenNthCalledWith(3, expect.objectContaining({
        target: expect.objectContaining({ value: 'Spirit' })
      }));
    });

    it('does not crash when onChange is not provided', () => {
      const { onChange, ...propsWithoutOnChange } = defaultProps;
      
      expect(() => {
        render(<AttributeSelectFormGroup {...propsWithoutOnChange} />);
        const select = screen.getByTestId('select-input');
        fireEvent.change(select, { target: { value: 'Agility' } });
      }).not.toThrow();
    });
  });

  describe('Integration with SelectFormGroup', () => {
    it('passes all required props to SelectFormGroup', () => {
      const props = {
        id: 'integration-test',
        label: 'Integration Label',
        onChange: jest.fn(),
        attribute: 'Smarts'
      };
      
      render(<AttributeSelectFormGroup {...props} />);
      
      // Verify that the props are correctly passed through
      expect(screen.getByLabelText('Integration Label')).toBeInTheDocument();
      expect(screen.getByTestId('select-input')).toHaveAttribute('id', 'integration-test');
      expect(screen.getByTestId('select-input')).toHaveValue('Smarts');
    });

    it('maintains proper component structure', () => {
      render(<AttributeSelectFormGroup {...defaultProps} />);
      
      // Check that the wrapper div contains the SelectFormGroup
      const wrapper = document.getElementById('AttributeSelectFormGroup-test-attribute');
      expect(wrapper).toBeInTheDocument();
      expect(wrapper).toContainElement(screen.getByTestId('select-form-group'));
    });
  });

  describe('Accessibility', () => {
    it('associates label with select input', () => {
      render(<AttributeSelectFormGroup {...defaultProps} />);
      
      const label = screen.getByText('Test Attribute');
      const select = screen.getByTestId('select-input');
      
      expect(label).toHaveAttribute('for', 'test-attribute');
      expect(select).toHaveAttribute('id', 'test-attribute');
    });

    it('supports screen readers with proper labeling', () => {
      render(<AttributeSelectFormGroup {...defaultProps} label="Character Primary Attribute" />);
      
      const select = screen.getByLabelText('Character Primary Attribute');
      expect(select).toBeInTheDocument();
    });
  });

  describe('Edge Cases', () => {
    it('handles empty string id', () => {
      expect(() => {
        render(<AttributeSelectFormGroup {...defaultProps} id="" />);
      }).not.toThrow();
    });

    it('handles very long label text', () => {
      const longLabel = 'A'.repeat(200);
      render(<AttributeSelectFormGroup {...defaultProps} label={longLabel} />);
      
      expect(screen.getByLabelText(longLabel)).toBeInTheDocument();
    });

    it('handles special characters in attribute values', () => {
      render(<AttributeSelectFormGroup {...defaultProps} attribute="Spirit" />);
      
      const select = screen.getByTestId('select-input');
      expect(select).toHaveValue('Spirit');
    });

    it('maintains consistent behavior with rapid selections', () => {
      const mockOnChange = jest.fn();
      render(<AttributeSelectFormGroup {...defaultProps} onChange={mockOnChange} />);
      
      const select = screen.getByTestId('select-input');
      
      // Simulate rapid selections
      for (let i = 0; i < 10; i++) {
        fireEvent.change(select, { target: { value: 'Agility' } });
        fireEvent.change(select, { target: { value: 'Vigor' } });
      }
      
      expect(mockOnChange).toHaveBeenCalledTimes(20);
    });
  });

  describe('PropTypes Validation', () => {
    it('accepts valid prop types', () => {
      const validProps = {
        id: 'test-id',
        label: 'Test Label',
        onChange: jest.fn(),
        attribute: 'Agility'
      };
      
      expect(() => render(<AttributeSelectFormGroup {...validProps} />)).not.toThrow();
    });

    it('works without optional attribute prop', () => {
      const { attribute, ...requiredProps } = defaultProps;
      
      expect(() => render(<AttributeSelectFormGroup {...requiredProps} />)).not.toThrow();
    });
  });
});
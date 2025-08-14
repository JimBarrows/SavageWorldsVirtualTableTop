import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import AttributeComponent from './AttributeComponent';

describe('AttributeComponent', () => {
  const defaultProps = {
    label: 'Strength',
    value: 'd8',
    onChange: jest.fn(),
    name: 'strength'
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders without crashing', () => {
      render(<AttributeComponent {...defaultProps} />);
    });

    it('displays the label', () => {
      render(<AttributeComponent {...defaultProps} />);
      expect(screen.getByText('Strength')).toBeInTheDocument();
    });

    it('displays the current value', () => {
      render(<AttributeComponent {...defaultProps} />);
      expect(screen.getByDisplayValue('d8')).toBeInTheDocument();
    });

    it('renders with custom className', () => {
      const { container } = render(
        <AttributeComponent {...defaultProps} className="custom-class" />
      );
      expect(container.firstChild).toHaveClass('custom-class');
    });

    it('renders dice options', () => {
      render(<AttributeComponent {...defaultProps} />);
      const select = screen.getByRole('combobox');
      expect(select).toBeInTheDocument();
      
      const options = screen.getAllByRole('option');
      expect(options).toHaveLength(6); // d4, d6, d8, d10, d12, d12+1
    });
  });

  describe('Interaction', () => {
    it('calls onChange when value is changed', () => {
      render(<AttributeComponent {...defaultProps} />);
      const select = screen.getByRole('combobox');
      
      fireEvent.change(select, { target: { value: 'd10' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        expect.objectContaining({
          target: expect.objectContaining({
            name: 'strength',
            value: 'd10'
          })
        })
      );
    });

    it('handles empty value', () => {
      render(<AttributeComponent {...defaultProps} value="" />);
      expect(screen.getByRole('combobox').value).toBe('');
    });

    it('handles null value', () => {
      render(<AttributeComponent {...defaultProps} value={null} />);
      expect(screen.getByRole('combobox').value).toBe('');
    });
  });

  describe('Validation', () => {
    it('displays error message when provided', () => {
      render(<AttributeComponent {...defaultProps} error="Invalid attribute" />);
      expect(screen.getByText('Invalid attribute')).toBeInTheDocument();
    });

    it('adds error class when error exists', () => {
      const { container } = render(
        <AttributeComponent {...defaultProps} error="Error" />
      );
      expect(container.querySelector('.error')).toBeInTheDocument();
    });

    it('shows required indicator when required', () => {
      render(<AttributeComponent {...defaultProps} required />);
      expect(screen.getByText('*')).toBeInTheDocument();
    });
  });

  describe('Disabled State', () => {
    it('disables select when disabled prop is true', () => {
      render(<AttributeComponent {...defaultProps} disabled />);
      expect(screen.getByRole('combobox')).toBeDisabled();
    });

    it('does not call onChange when disabled', () => {
      render(<AttributeComponent {...defaultProps} disabled />);
      const select = screen.getByRole('combobox');
      
      fireEvent.change(select, { target: { value: 'd10' } });
      
      expect(defaultProps.onChange).not.toHaveBeenCalled();
    });
  });

  describe('Custom Options', () => {
    it('renders custom dice options when provided', () => {
      const customOptions = ['d4', 'd6', 'd8'];
      render(<AttributeComponent {...defaultProps} options={customOptions} />);
      
      const options = screen.getAllByRole('option');
      expect(options).toHaveLength(3);
    });

    it('includes d12+2 and d12+3 for exceptional attributes', () => {
      const extendedOptions = ['d4', 'd6', 'd8', 'd10', 'd12', 'd12+1', 'd12+2', 'd12+3'];
      render(<AttributeComponent {...defaultProps} options={extendedOptions} />);
      
      expect(screen.getByRole('option', { name: 'd12+2' })).toBeInTheDocument();
      expect(screen.getByRole('option', { name: 'd12+3' })).toBeInTheDocument();
    });
  });

  describe('Tooltip', () => {
    it('displays tooltip when provided', () => {
      render(
        <AttributeComponent 
          {...defaultProps} 
          tooltip="This represents physical strength" 
        />
      );
      expect(screen.getByTitle('This represents physical strength')).toBeInTheDocument();
    });
  });

  describe('Accessibility', () => {
    it('associates label with select element', () => {
      render(<AttributeComponent {...defaultProps} id="strength-attr" />);
      const select = screen.getByLabelText('Strength');
      expect(select).toBeInTheDocument();
    });

    it('has proper ARIA attributes', () => {
      render(
        <AttributeComponent 
          {...defaultProps} 
          ariaLabel="Select strength attribute"
        />
      );
      const select = screen.getByRole('combobox');
      expect(select).toHaveAttribute('aria-label', 'Select strength attribute');
    });

    it('announces errors to screen readers', () => {
      render(<AttributeComponent {...defaultProps} error="Invalid value" />);
      const errorElement = screen.getByText('Invalid value');
      expect(errorElement).toHaveAttribute('role', 'alert');
    });
  });

  describe('Edge Cases', () => {
    it('handles special characters in label', () => {
      render(<AttributeComponent {...defaultProps} label="Spirit & Mind" />);
      expect(screen.getByText('Spirit & Mind')).toBeInTheDocument();
    });

    it('handles very long labels gracefully', () => {
      const longLabel = 'This is a very long attribute label that might cause layout issues';
      render(<AttributeComponent {...defaultProps} label={longLabel} />);
      expect(screen.getByText(longLabel)).toBeInTheDocument();
    });

    it('preserves additional props passed to select', () => {
      render(
        <AttributeComponent 
          {...defaultProps} 
          data-testid="custom-test-id"
          tabIndex={5}
        />
      );
      const select = screen.getByRole('combobox');
      expect(select).toHaveAttribute('data-testid', 'custom-test-id');
      expect(select).toHaveAttribute('tabIndex', '5');
    });
  });
});
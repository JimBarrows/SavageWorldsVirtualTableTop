import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import AttributeComponent from './AttributeComponent';

describe('AttributeComponent', () => {
  const defaultProps = {
    id: 'strength',
    value: {
      dice: 'd8',
      bonus: 0
    },
    onChange: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders without crashing', () => {
      render(<AttributeComponent {...defaultProps} />);
    });

    it('displays the dice select with current value', () => {
      render(<AttributeComponent {...defaultProps} />);
      const select = screen.getByRole('combobox');
      expect(select.value).toBe('d8');
    });

    it('renders with custom className', () => {
      const { container } = render(
        <AttributeComponent {...defaultProps} className="custom-class" />
      );
      // The component renders a wrapper div with input-group class
      const wrapperElement = container.querySelector('.input-group');
      expect(wrapperElement).toBeInTheDocument();
    });

    it('renders dice options', () => {
      render(<AttributeComponent {...defaultProps} />);
      const select = screen.getByRole('combobox');
      expect(select).toBeInTheDocument();
      
      const options = screen.getAllByRole('option');
      expect(options.length).toBeGreaterThanOrEqual(5); // At least d4, d6, d8, d10, d12
    });
  });

  describe('Interaction', () => {
    it('calls onChange when dice value is changed', () => {
      render(<AttributeComponent {...defaultProps} />);
      const select = screen.getByRole('combobox');
      
      fireEvent.change(select, { target: { value: 'd10' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith({
        dice: 'd10',
        bonus: null
      });
    });

    it('shows bonus input when d12 is selected', () => {
      const d12Props = {
        ...defaultProps,
        value: { dice: 'd12', bonus: 2 }
      };
      render(<AttributeComponent {...d12Props} />);
      const bonusInput = screen.getByDisplayValue('2');
      expect(bonusInput).toBeInTheDocument();
      expect(bonusInput.type).toBe('number');
    });

    it('calls onChange when bonus is changed for d12', () => {
      const d12Props = {
        ...defaultProps,
        value: { dice: 'd12', bonus: 0 }
      };
      render(<AttributeComponent {...d12Props} />);
      const bonusInput = screen.getByDisplayValue('0');
      
      fireEvent.change(bonusInput, { target: { value: '3' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith({
        dice: 'd12',
        bonus: 3
      });
    });

    it('sets bonus to 0 when changing to d12', () => {
      render(<AttributeComponent {...defaultProps} />);
      const select = screen.getByRole('combobox');
      
      fireEvent.change(select, { target: { value: 'd12' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith({
        dice: 'd12',
        bonus: 0
      });
    });
  });

  describe('Props', () => {
    it('supports disabled state', () => {
      render(<AttributeComponent {...defaultProps} disabled />);
      expect(screen.getByRole('combobox')).toBeDisabled();
    });

    it('supports required attribute', () => {
      render(<AttributeComponent {...defaultProps} required />);
      expect(screen.getByRole('combobox')).toBeRequired();
    });

    it('renders prepend content', () => {
      const prepend = <span>Prepend</span>;
      render(<AttributeComponent {...defaultProps} prepend={prepend} />);
      expect(screen.getByText('Prepend')).toBeInTheDocument();
    });

    it('renders append content', () => {
      const append = <span>Append</span>;
      render(<AttributeComponent {...defaultProps} append={append} />);
      expect(screen.getByText('Append')).toBeInTheDocument();
    });

    it('renders children', () => {
      render(
        <AttributeComponent {...defaultProps}>
          <span>Child content</span>
        </AttributeComponent>
      );
      expect(screen.getByText('Child content')).toBeInTheDocument();
    });

    it('uses default value when not provided', () => {
      const propsWithoutValue = {
        id: 'test',
        onChange: jest.fn()
      };
      render(<AttributeComponent {...propsWithoutValue} />);
      const select = screen.getByRole('combobox');
      // The default value is d4
      expect(select.value).toBe('d4');
    });
  });

  describe('Disabled State', () => {
    it('disables select when disabled prop is true', () => {
      render(<AttributeComponent {...defaultProps} disabled />);
      expect(screen.getByRole('combobox')).toBeDisabled();
    });

    it('disables bonus input when disabled and d12', () => {
      const d12Props = {
        ...defaultProps,
        value: { dice: 'd12', bonus: 2 },
        disabled: true
      };
      render(<AttributeComponent {...d12Props} />);
      const bonusInput = screen.getByDisplayValue('2');
      expect(bonusInput).toBeDisabled();
    });
  });
});
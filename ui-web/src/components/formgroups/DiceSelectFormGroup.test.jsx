import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import DiceSelectFormGroup from './DiceSelectFormGroup';

// Mock bootstrap-react-components
jest.mock('bootstrap-react-components', () => ({
  FormGroup: ({ children, id, label, validationMessage }) => (
    <div id={id} className="form-group">
      {label && <label>{label}</label>}
      {children}
      {validationMessage && <div className="invalid-feedback">{validationMessage}</div>}
    </div>
  ),
  FormControl: ({ id, type, value, onChange, disabled }) => (
    <input
      id={id}
      type={type}
      value={value}
      onChange={onChange}
      disabled={disabled}
      className="form-control"
    />
  )
}));

// Mock the DiceSelect component
jest.mock('../DiceSelect', () => {
  return function MockDiceSelect({ id, onChange, value, required, disabled }) {
    return (
      <select
        id={id}
        onChange={onChange}
        value={value?.dice || ''}
        required={required}
        disabled={disabled}
        role="combobox"
      >
        <option value="d4">d4</option>
        <option value="d6">d6</option>
        <option value="d8">d8</option>
        <option value="d10">d10</option>
        <option value="d12">d12</option>
      </select>
    );
  };
});

describe('DiceSelectFormGroup', () => {
  const defaultProps = {
    id: 'test-dice',
    label: 'Test Dice',
    value: { dice: 'd6', bonus: null },
    onChange: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('should render with all props', () => {
      render(<DiceSelectFormGroup {...defaultProps} />);
      
      expect(screen.getByText('Test Dice')).toBeInTheDocument();
    });

    it('should render dice select component', () => {
      const { container } = render(<DiceSelectFormGroup {...defaultProps} />);
      
      // Check for DiceSelect component presence
      const formGroup = container.querySelector('#DiceSelectFormGroup-test-dice');
      expect(formGroup).toBeInTheDocument();
    });

    it('should render bonus input when dice is d12', () => {
      const props = {
        ...defaultProps,
        value: { dice: 'd12', bonus: 0 }
      };
      
      const { container } = render(<DiceSelectFormGroup {...props} />);
      
      const bonusInput = container.querySelector('input[type="number"]');
      expect(bonusInput).toBeInTheDocument();
      expect(bonusInput).toHaveValue(0);
    });

    it('should not render bonus input for non-d12 dice', () => {
      const props = {
        ...defaultProps,
        value: { dice: 'd8', bonus: null }
      };
      
      const { container } = render(<DiceSelectFormGroup {...props} />);
      
      const bonusInput = container.querySelector('input[type="number"]');
      expect(bonusInput).not.toBeInTheDocument();
    });
  });

  describe('Dice Changes', () => {
    it('should handle dice change to d12 and set bonus to 0', () => {
      const onChange = jest.fn();
      const { rerender } = render(
        <DiceSelectFormGroup {...defaultProps} onChange={onChange} />
      );
      
      // Simulate dice change by finding DiceSelect and triggering its onChange
      const diceSelect = screen.getByRole('combobox');
      fireEvent.change(diceSelect, { target: { value: 'd12' } });
      
      expect(onChange).toHaveBeenCalledWith({
        dice: 'd12',
        bonus: 0
      });
    });

    it('should handle dice change to non-d12 and set bonus to null', () => {
      const onChange = jest.fn();
      render(
        <DiceSelectFormGroup 
          {...defaultProps} 
          value={{ dice: 'd12', bonus: 2 }}
          onChange={onChange} 
        />
      );
      
      const diceSelect = screen.getByRole('combobox');
      fireEvent.change(diceSelect, { target: { value: 'd8' } });
      
      expect(onChange).toHaveBeenCalledWith({
        dice: 'd8',
        bonus: null
      });
    });
  });

  describe('Bonus Changes', () => {
    it('should handle bonus change for d12', () => {
      const onChange = jest.fn();
      const { container } = render(
        <DiceSelectFormGroup 
          {...defaultProps} 
          value={{ dice: 'd12', bonus: 0 }}
          onChange={onChange} 
        />
      );
      
      const bonusInput = container.querySelector('input[type="number"]');
      fireEvent.change(bonusInput, { target: { value: '3' } });
      
      expect(onChange).toHaveBeenCalledWith({
        dice: 'd12',
        bonus: 3
      });
    });
  });

  describe('Props Validation', () => {
    it('should handle disabled state', () => {
      const { container } = render(
        <DiceSelectFormGroup 
          {...defaultProps} 
          value={{ dice: 'd12', bonus: 0 }}
          disabled={true} 
        />
      );
      
      const bonusInput = container.querySelector('input[type="number"]');
      expect(bonusInput).toBeDisabled();
    });

    it('should handle required prop', () => {
      render(
        <DiceSelectFormGroup {...defaultProps} required={true} />
      );
      
      // FormGroup should show required indicator
      expect(screen.getByText('Test Dice')).toBeInTheDocument();
    });

    it('should handle validation state', () => {
      const { container } = render(
        <DiceSelectFormGroup 
          {...defaultProps} 
          valid={false}
          validationMessage="Invalid dice selection" 
        />
      );
      
      expect(screen.getByText('Invalid dice selection')).toBeInTheDocument();
    });
  });

  describe('Edge Cases', () => {
    it('should handle value with d4', () => {
      const { container } = render(
        <DiceSelectFormGroup 
          {...defaultProps} 
          value={{ dice: 'd4', bonus: null }} 
        />
      );
      
      const bonusInput = container.querySelector('input[type="number"]');
      expect(bonusInput).not.toBeInTheDocument();
    });

    it('should handle value with d6', () => {
      const { container } = render(
        <DiceSelectFormGroup 
          {...defaultProps} 
          value={{ dice: 'd6', bonus: null }} 
        />
      );
      
      const bonusInput = container.querySelector('input[type="number"]');
      expect(bonusInput).not.toBeInTheDocument();
    });

    it('should handle value with d8', () => {
      const { container } = render(
        <DiceSelectFormGroup 
          {...defaultProps} 
          value={{ dice: 'd8', bonus: null }} 
        />
      );
      
      const bonusInput = container.querySelector('input[type="number"]');
      expect(bonusInput).not.toBeInTheDocument();
    });

    it('should handle value with d10', () => {
      const { container } = render(
        <DiceSelectFormGroup 
          {...defaultProps} 
          value={{ dice: 'd10', bonus: null }} 
        />
      );
      
      const bonusInput = container.querySelector('input[type="number"]');
      expect(bonusInput).not.toBeInTheDocument();
    });

    it('should handle value with d12 and various bonuses', () => {
      const testCases = [0, 1, 2, 3, -1];
      
      testCases.forEach(bonus => {
        const { container } = render(
          <DiceSelectFormGroup 
            {...defaultProps} 
            value={{ dice: 'd12', bonus }} 
          />
        );
        
        const bonusInput = container.querySelector('input[type="number"]');
        expect(bonusInput).toHaveValue(bonus);
      });
    });
  });
});
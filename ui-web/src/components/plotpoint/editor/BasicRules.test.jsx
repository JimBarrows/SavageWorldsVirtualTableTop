import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import BasicRules from './BasicRules';

// Mock bootstrap-react-components
jest.mock('bootstrap-react-components', () => ({
  NumberFormGroup: ({ id, label, onChange, value, required }) => (
    <div className="form-group">
      <label htmlFor={id}>{label}</label>
      <input
        id={id}
        type="number"
        value={value}
        onChange={onChange}
        required={required}
        role="spinbutton"
        aria-label={label}
      />
    </div>
  )
}));

describe('BasicRules Component', () => {
  const defaultProps = {
    id: 'test-rules',
    onChange: jest.fn(),
    basicRules: {
      maximumAttributePoints: 5,
      maximumMajorHindrances: 1,
      maximumMinorHindrances: 2,
      maximumSkillPoints: 15
    }
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('should render the component with title', () => {
      render(<BasicRules {...defaultProps} />);
      
      expect(screen.getByText('Basic Rules')).toBeInTheDocument();
    });

    it('should render all input fields with correct labels', () => {
      render(<BasicRules {...defaultProps} />);
      
      expect(screen.getByLabelText('Maximum Attribute Points')).toBeInTheDocument();
      expect(screen.getByLabelText('Maximum Number of Major Hindrances')).toBeInTheDocument();
      expect(screen.getByLabelText('Maximum Number of Minor Hindrances')).toBeInTheDocument();
      expect(screen.getByLabelText('Maximum Skill Points')).toBeInTheDocument();
    });

    it('should render with default values', () => {
      render(<BasicRules {...defaultProps} />);
      
      expect(screen.getByLabelText('Maximum Attribute Points')).toHaveValue(5);
      expect(screen.getByLabelText('Maximum Number of Major Hindrances')).toHaveValue(1);
      expect(screen.getByLabelText('Maximum Number of Minor Hindrances')).toHaveValue(2);
      expect(screen.getByLabelText('Maximum Skill Points')).toHaveValue(15);
    });

    it('should render with custom values', () => {
      const customRules = {
        maximumAttributePoints: 10,
        maximumMajorHindrances: 2,
        maximumMinorHindrances: 4,
        maximumSkillPoints: 20
      };
      
      render(<BasicRules {...defaultProps} basicRules={customRules} />);
      
      expect(screen.getByLabelText('Maximum Attribute Points')).toHaveValue(10);
      expect(screen.getByLabelText('Maximum Number of Major Hindrances')).toHaveValue(2);
      expect(screen.getByLabelText('Maximum Number of Minor Hindrances')).toHaveValue(4);
      expect(screen.getByLabelText('Maximum Skill Points')).toHaveValue(20);
    });
  });

  describe('User Interactions', () => {
    it('should handle maximum attribute points change', () => {
      const onChange = jest.fn();
      render(<BasicRules {...defaultProps} onChange={onChange} />);
      
      const input = screen.getByLabelText('Maximum Attribute Points');
      fireEvent.change(input, { target: { value: '7' } });
      
      expect(onChange).toHaveBeenCalledTimes(1);
      expect(onChange).toHaveBeenCalledWith({
        maximumAttributePoints: 7,
        maximumMajorHindrances: 1,
        maximumMinorHindrances: 2,
        maximumSkillPoints: 15
      });
    });

    it('should handle maximum major hindrances change', () => {
      const onChange = jest.fn();
      render(<BasicRules {...defaultProps} onChange={onChange} />);
      
      const input = screen.getByLabelText('Maximum Number of Major Hindrances');
      fireEvent.change(input, { target: { value: '3' } });
      
      expect(onChange).toHaveBeenCalledWith({
        maximumAttributePoints: 5,
        maximumMajorHindrances: 3,
        maximumMinorHindrances: 2,
        maximumSkillPoints: 15
      });
    });

    it('should handle maximum minor hindrances change', () => {
      const onChange = jest.fn();
      render(<BasicRules {...defaultProps} onChange={onChange} />);
      
      const input = screen.getByLabelText('Maximum Number of Minor Hindrances');
      fireEvent.change(input, { target: { value: '4' } });
      
      expect(onChange).toHaveBeenCalledWith({
        maximumAttributePoints: 5,
        maximumMajorHindrances: 1,
        maximumMinorHindrances: 4,
        maximumSkillPoints: 15
      });
    });

    it('should handle maximum skill points change', () => {
      const onChange = jest.fn();
      render(<BasicRules {...defaultProps} onChange={onChange} />);
      
      const input = screen.getByLabelText('Maximum Skill Points');
      fireEvent.change(input, { target: { value: '20' } });
      
      expect(onChange).toHaveBeenCalledWith({
        maximumAttributePoints: 5,
        maximumMajorHindrances: 1,
        maximumMinorHindrances: 2,
        maximumSkillPoints: 20
      });
    });

    it('should handle multiple changes', () => {
      const onChange = jest.fn();
      render(<BasicRules {...defaultProps} onChange={onChange} />);
      
      fireEvent.change(screen.getByLabelText('Maximum Attribute Points'), { target: { value: '8' } });
      fireEvent.change(screen.getByLabelText('Maximum Skill Points'), { target: { value: '25' } });
      
      expect(onChange).toHaveBeenCalledTimes(2);
      expect(onChange).toHaveBeenNthCalledWith(1, {
        maximumAttributePoints: 8,
        maximumMajorHindrances: 1,
        maximumMinorHindrances: 2,
        maximumSkillPoints: 15
      });
      expect(onChange).toHaveBeenNthCalledWith(2, {
        maximumAttributePoints: 5,
        maximumMajorHindrances: 1,
        maximumMinorHindrances: 2,
        maximumSkillPoints: 25
      });
    });
  });

  describe('Props Validation', () => {
    it('should use default props when basicRules is not provided', () => {
      const { container } = render(
        <BasicRules 
          id="test" 
          onChange={jest.fn()} 
        />
      );
      
      expect(screen.getByLabelText('Maximum Attribute Points')).toHaveValue(5);
      expect(screen.getByLabelText('Maximum Number of Major Hindrances')).toHaveValue(1);
      expect(screen.getByLabelText('Maximum Number of Minor Hindrances')).toHaveValue(2);
      expect(screen.getByLabelText('Maximum Skill Points')).toHaveValue(15);
    });

    it('should handle onChange not being provided gracefully', () => {
      const { container } = render(
        <BasicRules 
          id="test" 
          basicRules={defaultProps.basicRules}
        />
      );
      
      const input = screen.getByLabelText('Maximum Attribute Points');
      expect(() => fireEvent.change(input, { target: { value: '10' } })).not.toThrow();
    });

    it('should generate correct component IDs', () => {
      const { container } = render(<BasicRules {...defaultProps} />);
      
      const mainDiv = container.querySelector('#BasicRulesComponent_-test-rules');
      expect(mainDiv).toBeInTheDocument();
      
      const attributeInput = container.querySelector('#BasicRulesComponent_-test-rules-MaximumAttributePoints');
      expect(attributeInput).toBeInTheDocument();
      
      const majorInput = container.querySelector('#BasicRulesComponent_-test-rules-MaximumMajorHindrances');
      expect(majorInput).toBeInTheDocument();
      
      const minorInput = container.querySelector('#BasicRulesComponent_-test-rules-MaximumMinorHindrances');
      expect(minorInput).toBeInTheDocument();
      
      const skillInput = container.querySelector('#BasicRulesComponent_-test-rules-MaximumSkillPoints');
      expect(skillInput).toBeInTheDocument();
    });
  });

  describe('Number Conversions', () => {
    it('should convert string values to integers', () => {
      const onChange = jest.fn();
      render(<BasicRules {...defaultProps} onChange={onChange} />);
      
      const input = screen.getByLabelText('Maximum Attribute Points');
      fireEvent.change(input, { target: { value: '10' } });
      
      expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
        maximumAttributePoints: 10 // Should be number, not string
      }));
      expect(typeof onChange.mock.calls[0][0].maximumAttributePoints).toBe('number');
    });

    it('should handle zero values', () => {
      const onChange = jest.fn();
      render(<BasicRules {...defaultProps} onChange={onChange} />);
      
      const input = screen.getByLabelText('Maximum Attribute Points');
      fireEvent.change(input, { target: { value: '0' } });
      
      expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
        maximumAttributePoints: 0
      }));
    });

    it('should handle negative values', () => {
      const onChange = jest.fn();
      render(<BasicRules {...defaultProps} onChange={onChange} />);
      
      const input = screen.getByLabelText('Maximum Skill Points');
      fireEvent.change(input, { target: { value: '-5' } });
      
      expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
        maximumSkillPoints: -5
      }));
    });
  });

  describe('Required Fields', () => {
    it('should mark all fields as required', () => {
      render(<BasicRules {...defaultProps} />);
      
      const inputs = screen.getAllByRole('spinbutton');
      inputs.forEach(input => {
        expect(input).toHaveAttribute('required');
      });
    });
  });
});
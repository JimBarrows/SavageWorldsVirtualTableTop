import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import RankSelectFormGroup from './RankSelectFormGroup';

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components', () => ({
  SelectFormGroup: function MockSelectFormGroup({ id, label, onChange, options, value, required }) {
    // Handle the case where no onChange is provided by using defaultValue instead of value
    const selectProps = onChange 
      ? { 
          value: value || '', 
          onChange: (e) => {
            // Call the onChange handler with the event
            onChange(e);
          }
        }
      : { defaultValue: value || '' };
      
    return (
      <div data-testid="select-form-group">
        <label htmlFor={id}>{label}</label>
        <select 
          id={id} 
          {...selectProps}
          data-testid="select-input"
          required={required}
        >
          <option value="">Select a rank</option>
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

describe('RankSelectFormGroup Component', () => {
  const defaultProps = {
    id: 'test-rank',
    label: 'Test Rank',
    onChange: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<RankSelectFormGroup {...defaultProps} />);
      
      expect(screen.getByTestId('select-form-group')).toBeInTheDocument();
      expect(screen.getByTestId('select-input')).toBeInTheDocument();
    });

    it('renders with correct wrapper id', () => {
      render(<RankSelectFormGroup {...defaultProps} id="my-rank" />);
      
      const wrapper = document.getElementById('RankFormGroupComponent_my-rank');
      expect(wrapper).toBeInTheDocument();
    });

    it('uses default label when not provided', () => {
      const { label, ...propsWithoutLabel } = defaultProps;
      render(<RankSelectFormGroup {...propsWithoutLabel} />);
      
      expect(screen.getByText('Rank')).toBeInTheDocument();
    });

    it('renders with custom label', () => {
      render(<RankSelectFormGroup {...defaultProps} label="Character Rank" />);
      
      expect(screen.getByText('Character Rank')).toBeInTheDocument();
    });

    it('passes correct id to SelectFormGroup', () => {
      render(<RankSelectFormGroup {...defaultProps} id="custom-id" />);
      
      const select = screen.getByTestId('select-input');
      expect(select).toHaveAttribute('id', 'RankFormGroupComponent_custom-id');
    });
  });

  describe('Rank Options', () => {
    it('renders all Savage Worlds ranks', () => {
      render(<RankSelectFormGroup {...defaultProps} />);
      
      const expectedRanks = [
        'Novice',
        'Seasoned', 
        'Veteran',
        'Heroic',
        'Legendary'
      ];

      expectedRanks.forEach(rank => {
        expect(screen.getByRole('option', { name: rank })).toBeInTheDocument();
      });
    });

    it('has correct option values matching labels', () => {
      render(<RankSelectFormGroup {...defaultProps} />);
      
      const noviceOption = screen.getByRole('option', { name: 'Novice' });
      const seasonedOption = screen.getByRole('option', { name: 'Seasoned' });
      const veteranOption = screen.getByRole('option', { name: 'Veteran' });
      const heroicOption = screen.getByRole('option', { name: 'Heroic' });
      const legendaryOption = screen.getByRole('option', { name: 'Legendary' });

      expect(noviceOption).toHaveAttribute('value', 'Novice');
      expect(seasonedOption).toHaveAttribute('value', 'Seasoned');
      expect(veteranOption).toHaveAttribute('value', 'Veteran');
      expect(heroicOption).toHaveAttribute('value', 'Heroic');
      expect(legendaryOption).toHaveAttribute('value', 'Legendary');
    });

    it('includes default empty option', () => {
      render(<RankSelectFormGroup {...defaultProps} />);
      
      expect(screen.getByRole('option', { name: 'Select a rank' })).toBeInTheDocument();
    });

    it('orders ranks from lowest to highest progression', () => {
      render(<RankSelectFormGroup {...defaultProps} />);
      
      const options = screen.getAllByRole('option');
      const rankOptions = options.slice(1); // Skip the "Select a rank" option
      
      const expectedOrder = ['Novice', 'Seasoned', 'Veteran', 'Heroic', 'Legendary'];
      rankOptions.forEach((option, index) => {
        expect(option).toHaveTextContent(expectedOrder[index]);
      });
    });
  });

  describe('Value Handling', () => {
    it('displays selected rank value', () => {
      render(<RankSelectFormGroup {...defaultProps} rank="Veteran" />);
      
      const select = screen.getByTestId('select-input');
      expect(select).toHaveValue('Veteran');
    });

    it('displays empty value when no rank is selected', () => {
      render(<RankSelectFormGroup {...defaultProps} />);
      
      const select = screen.getByTestId('select-input');
      expect(select).toHaveValue('');
    });

    it('handles undefined rank prop', () => {
      render(<RankSelectFormGroup {...defaultProps} rank={undefined} />);
      
      const select = screen.getByTestId('select-input');
      expect(select).toHaveValue('');
    });

    it('handles null rank prop', () => {
      render(<RankSelectFormGroup {...defaultProps} rank={null} />);
      
      const select = screen.getByTestId('select-input');
      expect(select).toHaveValue('');
    });

    it('handles all valid rank values', () => {
      const ranks = ['Novice', 'Seasoned', 'Veteran', 'Heroic', 'Legendary'];
      
      ranks.forEach(rank => {
        const { unmount } = render(<RankSelectFormGroup {...defaultProps} rank={rank} />);
        
        const select = screen.getByTestId('select-input');
        expect(select).toHaveValue(rank);
        
        unmount();
      });
    });
  });

  describe('Required Field Handling', () => {
    it('defaults to not required', () => {
      render(<RankSelectFormGroup {...defaultProps} />);
      
      const select = screen.getByTestId('select-input');
      expect(select).not.toHaveAttribute('required');
    });

    it('marks field as required when specified', () => {
      render(<RankSelectFormGroup {...defaultProps} required={true} />);
      
      const select = screen.getByTestId('select-input');
      expect(select).toHaveAttribute('required');
    });

    it('marks field as not required when explicitly set to false', () => {
      render(<RankSelectFormGroup {...defaultProps} required={false} />);
      
      const select = screen.getByTestId('select-input');
      expect(select).not.toHaveAttribute('required');
    });
  });

  describe('Change Handling', () => {
    it('calls onChange when rank is selected', () => {
      const mockOnChange = jest.fn();
      render(<RankSelectFormGroup {...defaultProps} onChange={mockOnChange} />);
      
      const select = screen.getByTestId('select-input');
      fireEvent.change(select, { target: { value: 'Heroic' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(1);
      expect(mockOnChange).toHaveBeenCalledWith(expect.objectContaining({
        type: 'change'
      }));
    });

    it('calls onChange when clearing selection', () => {
      const mockOnChange = jest.fn();
      render(<RankSelectFormGroup {...defaultProps} onChange={mockOnChange} rank="Legendary" />);
      
      const select = screen.getByTestId('select-input');
      fireEvent.change(select, { target: { value: '' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(1);
      expect(mockOnChange).toHaveBeenCalledWith(expect.objectContaining({
        type: 'change'
      }));
    });

    it('handles rank progression changes', () => {
      const mockOnChange = jest.fn();
      render(<RankSelectFormGroup {...defaultProps} onChange={mockOnChange} />);
      
      const select = screen.getByTestId('select-input');
      
      // Simulate character progression through ranks
      const progression = ['Novice', 'Seasoned', 'Veteran', 'Heroic', 'Legendary'];
      
      progression.forEach((rank) => {
        fireEvent.change(select, { target: { value: rank } });
      });
      
      expect(mockOnChange).toHaveBeenCalledTimes(5);
      expect(mockOnChange).toHaveBeenCalledWith(expect.objectContaining({
        type: 'change'
      }));
    });

    it('does not crash when onChange is not provided', () => {
      const { onChange, ...propsWithoutOnChange } = defaultProps;
      
      expect(() => {
        render(<RankSelectFormGroup {...propsWithoutOnChange} />);
        const select = screen.getByTestId('select-input');
        fireEvent.change(select, { target: { value: 'Veteran' } });
      }).not.toThrow();
    });
  });

  describe('Integration with SelectFormGroup', () => {
    it('passes all required props to SelectFormGroup', () => {
      const props = {
        id: 'integration-test',
        label: 'Integration Label',
        onChange: jest.fn(),
        rank: 'Seasoned',
        required: true
      };
      
      render(<RankSelectFormGroup {...props} />);
      
      // Verify that the props are correctly passed through
      expect(screen.getByText('Integration Label')).toBeInTheDocument();
      expect(screen.getByTestId('select-input')).toHaveAttribute('id', 'RankFormGroupComponent_integration-test');
      expect(screen.getByTestId('select-input')).toHaveValue('Seasoned');
      expect(screen.getByTestId('select-input')).toHaveAttribute('required');
    });

    it('maintains proper component structure', () => {
      render(<RankSelectFormGroup {...defaultProps} />);
      
      // Check that the wrapper div contains the SelectFormGroup
      const wrapper = document.getElementById('RankFormGroupComponent_test-rank');
      expect(wrapper).toBeInTheDocument();
      expect(wrapper).toContainElement(screen.getByTestId('select-form-group'));
    });
  });

  describe('Accessibility', () => {
    it('associates label with select input', () => {
      render(<RankSelectFormGroup {...defaultProps} />);
      
      const label = screen.getByText('Test Rank');
      const select = screen.getByTestId('select-input');
      
      expect(label).toHaveAttribute('for', 'RankFormGroupComponent_test-rank');
      expect(select).toHaveAttribute('id', 'RankFormGroupComponent_test-rank');
    });

    it('supports screen readers with proper labeling', () => {
      render(<RankSelectFormGroup {...defaultProps} label="Character Experience Rank" />);
      
      const select = screen.getByTestId('select-input');
      expect(select).toHaveAttribute('id', 'RankFormGroupComponent_test-rank');
      expect(screen.getByText('Character Experience Rank')).toBeInTheDocument();
    });

    it('indicates required fields for assistive technology', () => {
      render(<RankSelectFormGroup {...defaultProps} required={true} />);
      
      const select = screen.getByTestId('select-input');
      expect(select).toHaveAttribute('required');
    });
  });

  describe('Game Logic Validation', () => {
    it('supports character advancement mechanics', () => {
      render(<RankSelectFormGroup {...defaultProps} />);
      
      // Test that all advancement ranks are available
      const advancementRanks = ['Novice', 'Seasoned', 'Veteran', 'Heroic', 'Legendary'];
      
      advancementRanks.forEach(rank => {
        const option = screen.getByRole('option', { name: rank });
        expect(option).toHaveAttribute('value', rank);
      });
    });

    it('maintains Savage Worlds rank hierarchy', () => {
      render(<RankSelectFormGroup {...defaultProps} />);
      
      const options = screen.getAllByRole('option');
      const rankOptions = options.slice(1); // Skip empty option
      
      // Verify the ranks are in the correct hierarchical order
      expect(rankOptions[0]).toHaveTextContent('Novice'); // Starting rank
      expect(rankOptions[1]).toHaveTextContent('Seasoned'); // First advancement
      expect(rankOptions[2]).toHaveTextContent('Veteran'); // Second advancement
      expect(rankOptions[3]).toHaveTextContent('Heroic'); // Third advancement
      expect(rankOptions[4]).toHaveTextContent('Legendary'); // Final advancement
    });
  });

  describe('Edge Cases', () => {
    it('handles empty string id', () => {
      expect(() => {
        render(<RankSelectFormGroup {...defaultProps} id="" />);
      }).not.toThrow();
    });

    it('handles very long label text', () => {
      const longLabel = 'Character Experience Rank for Advanced Campaign Settings'.repeat(10);
      render(<RankSelectFormGroup {...defaultProps} label={longLabel} />);
      
      expect(screen.getByText(longLabel)).toBeInTheDocument();
    });

    it('handles invalid rank values gracefully', () => {
      expect(() => {
        render(<RankSelectFormGroup {...defaultProps} rank="InvalidRank" />);
      }).not.toThrow();
      
      // The component should render without crashing, even with invalid values
      expect(screen.getByTestId('select-input')).toBeInTheDocument();
    });

    it('maintains state consistency during rapid changes', () => {
      const mockOnChange = jest.fn();
      render(<RankSelectFormGroup {...defaultProps} onChange={mockOnChange} />);
      
      const select = screen.getByTestId('select-input');
      
      // Simulate rapid rank changes
      const ranks = ['Novice', 'Veteran', 'Seasoned', 'Legendary', 'Heroic'];
      ranks.forEach(rank => {
        fireEvent.change(select, { target: { value: rank } });
      });
      
      expect(mockOnChange).toHaveBeenCalledTimes(5);
      expect(mockOnChange).toHaveBeenCalledWith(expect.objectContaining({
        type: 'change'
      }));
    });
  });

  describe('PropTypes Validation', () => {
    it('accepts valid prop types', () => {
      const validProps = {
        id: 'test-id',
        label: 'Test Label',
        onChange: jest.fn(),
        rank: 'Veteran',
        required: true
      };
      
      expect(() => render(<RankSelectFormGroup {...validProps} />)).not.toThrow();
    });

    it('works without optional props', () => {
      const { rank, required, onChange, ...requiredProps } = defaultProps;
      
      expect(() => render(<RankSelectFormGroup {...requiredProps} />)).not.toThrow();
    });

    it('uses default props when not provided', () => {
      const { label, ...propsWithoutDefaults } = defaultProps;
      render(<RankSelectFormGroup {...propsWithoutDefaults} />);
      
      // Should use default label
      expect(screen.getByText('Rank')).toBeInTheDocument();
    });
  });

  describe('Component State Management', () => {
    it('maintains component identity across rerenders', () => {
      const { rerender } = render(<RankSelectFormGroup {...defaultProps} rank="Novice" />);
      
      const initialWrapper = document.getElementById('RankFormGroupComponent_test-rank');
      
      rerender(<RankSelectFormGroup {...defaultProps} rank="Veteran" />);
      
      const rerenderedWrapper = document.getElementById('RankFormGroupComponent_test-rank');
      expect(rerenderedWrapper).toBe(initialWrapper);
    });

    it('updates displayed value when rank prop changes', () => {
      const { rerender } = render(<RankSelectFormGroup {...defaultProps} rank="Novice" />);
      
      let select = screen.getByTestId('select-input');
      expect(select).toHaveValue('Novice');
      
      rerender(<RankSelectFormGroup {...defaultProps} rank="Legendary" />);
      
      select = screen.getByTestId('select-input');
      expect(select).toHaveValue('Legendary');
    });
  });
});
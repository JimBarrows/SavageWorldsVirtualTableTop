import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import SelectedHindranceEditor from './Editor';

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components', () => ({
  TextAreaFormGroup: function MockTextAreaFormGroup({ id, label, onChange, value }) {
    return (
      <div data-testid="textarea-form-group">
        <label htmlFor={id}>{label}</label>
        <textarea 
          id={id} 
          value={value || ''} 
          onChange={onChange}
          data-testid="textarea-input"
        />
      </div>
    );
  }
}));

describe('SelectedHindranceEditor Component', () => {
  const baseMockHindrance = {
    hindrance: {
      name: 'Test Hindrance',
      description: 'This is a test hindrance description.',
      severity: 'Major'
    },
    note: 'Test note',
    severity: 'Major'
  };

  const defaultProps = {
    id: 'test-hindrance',
    hindrance: baseMockHindrance,
    onChange: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<SelectedHindranceEditor {...defaultProps} />);
      
      expect(screen.getByText('Test Hindrance')).toBeInTheDocument();
      expect(screen.getByText('This is a test hindrance description.')).toBeInTheDocument();
    });

    it('renders with correct wrapper id', () => {
      render(<SelectedHindranceEditor {...defaultProps} id="custom-id" />);
      
      const wrapper = document.getElementById('SelectedHindranceEditor-custom-id');
      expect(wrapper).toBeInTheDocument();
    });

    it('displays hindrance name as heading', () => {
      const hindrance = {
        ...baseMockHindrance,
        hindrance: {
          ...baseMockHindrance.hindrance,
          name: 'Bloodthirsty'
        }
      };

      render(<SelectedHindranceEditor {...defaultProps} hindrance={hindrance} />);
      
      expect(screen.getByRole('heading', { level: 4 })).toHaveTextContent('Bloodthirsty');
    });

    it('displays hindrance description', () => {
      const hindrance = {
        ...baseMockHindrance,
        hindrance: {
          ...baseMockHindrance.hindrance,
          description: 'Character never takes prisoners and always fights to the death.'
        }
      };

      render(<SelectedHindranceEditor {...defaultProps} hindrance={hindrance} />);
      
      expect(screen.getByText('Character never takes prisoners and always fights to the death.')).toBeInTheDocument();
    });

    it('renders note textarea with correct configuration', () => {
      render(<SelectedHindranceEditor {...defaultProps} />);
      
      expect(screen.getByTestId('textarea-form-group')).toBeInTheDocument();
      expect(screen.getByLabelText('Note')).toBeInTheDocument();
      expect(screen.getByTestId('textarea-input')).toHaveValue('Test note');
    });
  });

  describe('Fixed Severity Hindrances', () => {
    it('displays fixed severity as label when not Major or Minor', () => {
      const hindrance = {
        ...baseMockHindrance,
        hindrance: {
          ...baseMockHindrance.hindrance,
          severity: 'Major'
        },
        severity: 'Major'
      };

      render(<SelectedHindranceEditor {...defaultProps} hindrance={hindrance} />);
      
      expect(screen.getByText('Severity: Major')).toBeInTheDocument();
      expect(screen.queryByRole('radio')).not.toBeInTheDocument();
    });

    it('displays fixed minor severity correctly', () => {
      const hindrance = {
        ...baseMockHindrance,
        hindrance: {
          ...baseMockHindrance.hindrance,
          severity: 'Minor'
        },
        severity: 'Minor'
      };

      render(<SelectedHindranceEditor {...defaultProps} hindrance={hindrance} />);
      
      expect(screen.getByText('Severity: Minor')).toBeInTheDocument();
    });

    it('falls back to hindrance.hindrance.severity when severity is not set', () => {
      const hindrance = {
        hindrance: {
          name: 'Test Hindrance',
          description: 'Test description',
          severity: 'Major'
        },
        note: ''
        // no severity property
      };

      render(<SelectedHindranceEditor {...defaultProps} hindrance={hindrance} />);
      
      expect(screen.getByText('Severity: Major')).toBeInTheDocument();
    });
  });

  describe('Variable Severity Hindrances', () => {
    const variableSeverityHindrance = {
      hindrance: {
        name: 'Curious',
        description: 'Character investigates strange sounds, noises, etc.',
        severity: 'Major or Minor'
      },
      note: 'Character note',
      severity: 'Major'
    };

    it('renders radio buttons for Major or Minor severity', () => {
      render(<SelectedHindranceEditor {...defaultProps} hindrance={variableSeverityHindrance} />);
      
      expect(screen.getByRole('radio', { name: /Major/ })).toBeInTheDocument();
      expect(screen.getByRole('radio', { name: /Minor/ })).toBeInTheDocument();
    });

    it('checks the correct radio button based on current severity', () => {
      render(<SelectedHindranceEditor {...defaultProps} hindrance={variableSeverityHindrance} />);
      
      const majorRadio = screen.getByRole('radio', { name: /Major/ });
      const minorRadio = screen.getByRole('radio', { name: /Minor/ });
      
      expect(majorRadio).toBeChecked();
      expect(minorRadio).not.toBeChecked();
    });

    it('checks Minor radio when severity is Minor', () => {
      const minorHindrance = {
        ...variableSeverityHindrance,
        severity: 'Minor'
      };

      render(<SelectedHindranceEditor {...defaultProps} hindrance={minorHindrance} />);
      
      const majorRadio = screen.getByRole('radio', { name: /Major/ });
      const minorRadio = screen.getByRole('radio', { name: /Minor/ });
      
      expect(majorRadio).not.toBeChecked();
      expect(minorRadio).toBeChecked();
    });

    it('uses unique radio button names for different component instances', () => {
      const { unmount } = render(<SelectedHindranceEditor {...defaultProps} id="first" hindrance={variableSeverityHindrance} />);
      
      const firstMajorRadio = screen.getByRole('radio', { name: /Major/ });
      expect(firstMajorRadio).toHaveAttribute('name', 'severity-first');
      
      unmount();
      
      render(<SelectedHindranceEditor {...defaultProps} id="second" hindrance={variableSeverityHindrance} />);
      
      const secondMajorRadio = screen.getByRole('radio', { name: /Major/ });
      expect(secondMajorRadio).toHaveAttribute('name', 'severity-second');
    });

    it('calls onChange when Major radio is selected', () => {
      const minorHindrance = {
        ...variableSeverityHindrance,
        severity: 'Minor'
      };
      const mockOnChange = jest.fn();

      render(<SelectedHindranceEditor {...defaultProps} hindrance={minorHindrance} onChange={mockOnChange} />);
      
      const majorRadio = screen.getByRole('radio', { name: /Major/ });
      fireEvent.click(majorRadio);
      
      expect(mockOnChange).toHaveBeenCalledWith({
        ...minorHindrance,
        severity: 'Major'
      });
    });

    it('calls onChange when Minor radio is selected', () => {
      const mockOnChange = jest.fn();

      render(<SelectedHindranceEditor {...defaultProps} hindrance={variableSeverityHindrance} onChange={mockOnChange} />);
      
      const minorRadio = screen.getByRole('radio', { name: /Minor/ });
      fireEvent.click(minorRadio);
      
      expect(mockOnChange).toHaveBeenCalledWith({
        ...variableSeverityHindrance,
        severity: 'Minor'
      });
    });

    it('does not crash when onChange is not provided', () => {
      const { onChange, ...propsWithoutOnChange } = defaultProps;

      expect(() => {
        render(<SelectedHindranceEditor {...propsWithoutOnChange} hindrance={variableSeverityHindrance} />);
        
        const majorRadio = screen.getByRole('radio', { name: /Major/ });
        fireEvent.click(majorRadio);
      }).not.toThrow();
    });
  });

  describe('Note Handling', () => {
    it('displays current note value in textarea', () => {
      const hindrance = {
        ...baseMockHindrance,
        note: 'This is my custom note about the hindrance'
      };

      render(<SelectedHindranceEditor {...defaultProps} hindrance={hindrance} />);
      
      expect(screen.getByTestId('textarea-input')).toHaveValue('This is my custom note about the hindrance');
    });

    it('handles empty note gracefully', () => {
      const hindrance = {
        ...baseMockHindrance,
        note: ''
      };

      render(<SelectedHindranceEditor {...defaultProps} hindrance={hindrance} />);
      
      expect(screen.getByTestId('textarea-input')).toHaveValue('');
    });

    it('handles undefined note gracefully', () => {
      const hindrance = {
        ...baseMockHindrance
        // no note property
      };
      delete hindrance.note;

      render(<SelectedHindranceEditor {...defaultProps} hindrance={hindrance} />);
      
      expect(screen.getByTestId('textarea-input')).toHaveValue('');
    });

    it('calls onChange when note is modified', () => {
      const mockOnChange = jest.fn();

      render(<SelectedHindranceEditor {...defaultProps} onChange={mockOnChange} />);
      
      const textarea = screen.getByTestId('textarea-input');
      fireEvent.change(textarea, { target: { value: 'Updated note content' } });
      
      expect(mockOnChange).toHaveBeenCalledWith({
        ...baseMockHindrance,
        note: 'Updated note content'
      });
    });

    it('preserves other hindrance properties when updating note', () => {
      const complexHindrance = {
        ...baseMockHindrance,
        customProperty: 'custom value',
        anotherProperty: 123
      };
      const mockOnChange = jest.fn();

      render(<SelectedHindranceEditor {...defaultProps} hindrance={complexHindrance} onChange={mockOnChange} />);
      
      const textarea = screen.getByTestId('textarea-input');
      fireEvent.change(textarea, { target: { value: 'New note' } });
      
      expect(mockOnChange).toHaveBeenCalledWith({
        ...complexHindrance,
        note: 'New note'
      });
    });

    it('does not crash when onChange is not provided for note changes', () => {
      const { onChange, ...propsWithoutOnChange } = defaultProps;

      expect(() => {
        render(<SelectedHindranceEditor {...propsWithoutOnChange} />);
        
        const textarea = screen.getByTestId('textarea-input');
        fireEvent.change(textarea, { target: { value: 'New note' } });
      }).not.toThrow();
    });
  });

  describe('Savage Worlds Game Logic', () => {
    it('handles typical Major hindrances correctly', () => {
      const majorHindrances = [
        { name: 'Bloodthirsty', severity: 'Major' },
        { name: 'Death Wish', severity: 'Major' },
        { name: 'Enemy', severity: 'Major' }
      ];

      majorHindrances.forEach(hindranceData => {
        const hindrance = {
          hindrance: {
            ...hindranceData,
            description: 'Test description'
          },
          note: '',
          severity: 'Major'
        };

        const { unmount } = render(<SelectedHindranceEditor {...defaultProps} hindrance={hindrance} />);
        
        expect(screen.getByText(hindranceData.name)).toBeInTheDocument();
        expect(screen.getByText('Severity: Major')).toBeInTheDocument();
        
        unmount();
      });
    });

    it('handles typical Minor hindrances correctly', () => {
      const minorHindrances = [
        { name: 'All Thumbs', severity: 'Minor' },
        { name: 'Anemic', severity: 'Minor' },
        { name: 'Bad Eyes', severity: 'Minor' }
      ];

      minorHindrances.forEach(hindranceData => {
        const hindrance = {
          hindrance: {
            ...hindranceData,
            description: 'Test description'
          },
          note: '',
          severity: 'Minor'
        };

        const { unmount } = render(<SelectedHindranceEditor {...defaultProps} hindrance={hindrance} />);
        
        expect(screen.getByText(hindranceData.name)).toBeInTheDocument();
        expect(screen.getByText('Severity: Minor')).toBeInTheDocument();
        
        unmount();
      });
    });

    it('handles variable severity hindrances from Savage Worlds', () => {
      const variableHindrances = [
        'Curious',
        'Habit',
        'Loyal',
        'Phobia',
        'Quirk',
        'Stubborn',
        'Vow'
      ];

      variableHindrances.forEach(name => {
        const hindrance = {
          hindrance: {
            name: name,
            description: 'Variable severity hindrance',
            severity: 'Major or Minor'
          },
          note: '',
          severity: 'Major'
        };

        const { unmount } = render(<SelectedHindranceEditor {...defaultProps} hindrance={hindrance} />);
        
        expect(screen.getByText(name)).toBeInTheDocument();
        expect(screen.getByRole('radio', { name: /Major/ })).toBeInTheDocument();
        expect(screen.getByRole('radio', { name: /Minor/ })).toBeInTheDocument();
        
        unmount();
      });
    });

    it('supports character development through hindrance notes', () => {
      const developmentNote = 'Character overcame this fear during the battle at Thunder Ridge. Now only applies in underground settings.';
      const hindrance = {
        ...baseMockHindrance,
        note: developmentNote
      };

      render(<SelectedHindranceEditor {...defaultProps} hindrance={hindrance} />);
      
      expect(screen.getByTestId('textarea-input')).toHaveValue(developmentNote);
    });
  });

  describe('Accessibility', () => {
    it('associates labels with radio buttons correctly', () => {
      const variableSeverityHindrance = {
        hindrance: {
          name: 'Curious',
          description: 'Test description',
          severity: 'Major or Minor'
        },
        note: '',
        severity: 'Major'
      };

      render(<SelectedHindranceEditor {...defaultProps} hindrance={variableSeverityHindrance} />);
      
      const majorRadio = screen.getByRole('radio', { name: /Major/ });
      const minorRadio = screen.getByRole('radio', { name: /Minor/ });
      
      expect(majorRadio).toBeInTheDocument();
      expect(minorRadio).toBeInTheDocument();
    });

    it('provides accessible textarea labeling', () => {
      render(<SelectedHindranceEditor {...defaultProps} />);
      
      const textarea = screen.getByLabelText('Note');
      expect(textarea).toBeInTheDocument();
    });

    it('uses semantic heading for hindrance name', () => {
      render(<SelectedHindranceEditor {...defaultProps} />);
      
      const heading = screen.getByRole('heading', { level: 4 });
      expect(heading).toHaveTextContent('Test Hindrance');
    });
  });

  describe('Edge Cases', () => {
    it('handles malformed hindrance data gracefully', () => {
      const malformedHindrance = {
        hindrance: {
          // missing name and description
          severity: 'Major'
        },
        note: 'test'
      };

      expect(() => {
        render(<SelectedHindranceEditor {...defaultProps} hindrance={malformedHindrance} />);
      }).not.toThrow();
    });

    it('handles very long hindrance names', () => {
      const longNameHindrance = {
        ...baseMockHindrance,
        hindrance: {
          ...baseMockHindrance.hindrance,
          name: 'A'.repeat(200)
        }
      };

      render(<SelectedHindranceEditor {...defaultProps} hindrance={longNameHindrance} />);
      
      const heading = screen.getByRole('heading', { level: 4 });
      expect(heading).toHaveTextContent('A'.repeat(200));
    });

    it('handles very long descriptions', () => {
      const longDescription = 'Very long description that spans multiple lines and contains detailed information about the hindrance effects and how it impacts character behavior in various scenarios.'.repeat(10);
      const longDescriptionHindrance = {
        ...baseMockHindrance,
        hindrance: {
          ...baseMockHindrance.hindrance,
          description: longDescription
        }
      };

      render(<SelectedHindranceEditor {...defaultProps} hindrance={longDescriptionHindrance} />);
      
      expect(screen.getByText(longDescription)).toBeInTheDocument();
    });

    it('handles rapid severity changes', () => {
      const variableSeverityHindrance = {
        hindrance: {
          name: 'Curious',
          description: 'Test description',
          severity: 'Major or Minor'
        },
        note: '',
        severity: 'Major'
      };
      const mockOnChange = jest.fn();

      render(<SelectedHindranceEditor {...defaultProps} hindrance={variableSeverityHindrance} onChange={mockOnChange} />);
      
      const majorRadio = screen.getByRole('radio', { name: /Major/ });
      const minorRadio = screen.getByRole('radio', { name: /Minor/ });
      
      // Rapid clicking
      fireEvent.click(minorRadio);
      fireEvent.click(majorRadio);
      fireEvent.click(minorRadio);
      
      expect(mockOnChange).toHaveBeenCalledTimes(3);
      expect(mockOnChange).toHaveBeenLastCalledWith({
        ...variableSeverityHindrance,
        severity: 'Minor'
      });
    });

    it('maintains component stability during prop updates', () => {
      const { rerender } = render(<SelectedHindranceEditor {...defaultProps} />);
      
      const originalWrapper = document.getElementById('SelectedHindranceEditor-test-hindrance');
      
      const updatedHindrance = {
        ...baseMockHindrance,
        note: 'Updated note'
      };
      
      rerender(<SelectedHindranceEditor {...defaultProps} hindrance={updatedHindrance} />);
      
      const updatedWrapper = document.getElementById('SelectedHindranceEditor-test-hindrance');
      expect(updatedWrapper).toBe(originalWrapper);
      expect(screen.getByTestId('textarea-input')).toHaveValue('Updated note');
    });
  });

  describe('PropTypes Validation', () => {
    it('accepts valid prop types', () => {
      const validProps = {
        id: 'valid-id',
        hindrance: {
          hindrance: {
            name: 'Test Hindrance',
            description: 'Test description',
            severity: 'Major'
          },
          note: 'Test note',
          severity: 'Major'
        },
        onChange: jest.fn()
      };
      
      expect(() => render(<SelectedHindranceEditor {...validProps} />)).not.toThrow();
    });

    it('works without optional onChange prop', () => {
      const { onChange, ...requiredProps } = defaultProps;
      
      expect(() => render(<SelectedHindranceEditor {...requiredProps} />)).not.toThrow();
    });
  });
});